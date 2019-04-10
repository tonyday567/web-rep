{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
 
import Control.Lens hiding (Wrapped, Unwrapped)
import Control.Category (id)
import Data.Aeson (Value)
import Data.Attoparsec.Text
import Data.HashMap.Strict
import Lucid hiding (b_, button_)
import Network.JavaScript
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Network.Wai.Middleware.RequestLogger
import Protolude hiding (replace, empty, Rep, log)
import Web.Page
import Web.Page.Bootstrap
import Web.Page.Examples
import Web.Page.Html.Input
import Web.Page.Bridge
import Web.Page.Bridge.Rep
import Web.Scotty
import qualified Box
import qualified Control.Exception as E
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Options.Generic
import Network.Wai

testPage :: Html () -> Page
testPage h =
  showJs <>
  bootstrapPage <>
  bridgePage &
  #htmlHeader .~ title_ "testPage" &
  #htmlBody .~ b_ "container"
  (mconcat
    [ b_ "row" (h1_ "testPage")
    , b_ "row" (with div_ [id_ "test"] h)
    , b_ "row" (with div_ [id_ "log"] (h2_ "server log"))
    ])

ioTestPage :: Html () -> Html () -> Page
ioTestPage i r =
  showJs <>
  bootstrapPage <>
  bridgePage &
  #htmlHeader .~ title_ "ioTestPage" &
  #htmlBody .~ b_ "container"
  (mconcat
    [ b_ "row" (h1_ "input-output test page")
    , b_ "row" (b_ "col-sm" (h2_ "inputs" <> with form_ [id_ "inputs"] i) <>
           b_ "col-sm" (with div_ [id_ "output"]
                  (h2_ "output" <>
                   with div_ [id_ "results"] r)))
    , b_ "row" (with div_ [id_ "log"] (h2_ "server log"))
    ])

rangeTest :: Input Int
rangeTest = bridgeify $ bootify $
  Input
  3
  Slider
  (Just "range example")
  Nothing
  "rangeid"
  [ ("style", "max-width:15rem;")
  , ("min", "0")
  , ("max", "5")
  , ("step", "1")
  ]

textTest :: Input Text
textTest = bridgeify $ bootify $
  Input
  "abc"
  TextBox
  (Just "label")
  Nothing
  "textid"
  [ ("style", "max-width:15rem;")
  , ("placeholder", "test placeholder")
  ]

initBridgeTest :: (Int, Text)
initBridgeTest = (rangeTest ^. #val, textTest ^. #val)

stepBridgeTest :: Element -> (Int, Text) -> Either Text (Int, Text)
stepBridgeTest (Element "rangeid" v) (_, t) =
  either
  (Left . Text.pack)
  (\x -> Right (x,t))
  p
  where
    p = parseOnly decimal v
stepBridgeTest (Element "textid" v) (n, _) = Right (n,v)
stepBridgeTest e _ = Left $ "unknown id: " <> show e

stepBridgeTest' :: Element -> (Int, Text) -> (Int,Text)
stepBridgeTest' e s =
  case stepBridgeTest e s of
    Left _ -> s
    Right x -> x

sendBridgeTest :: (Show a) => Engine -> Either Text a -> IO ()
sendBridgeTest e (Left err) = append e "log" err
sendBridgeTest e (Right a) =
  replace e "results"
  (Lazy.toStrict $ renderText $ cardify [] mempty (Just "result")
    (toHtml  (show a :: Text)))

consumeBridgeTest :: Event Value -> Engine -> IO (Int, Text)
consumeBridgeTest ev e =
  elementConsume initBridgeTest stepBridgeTest'
  ( (Box.liftC <$> Box.showStdout) <>
    pure (Box.Committer (\v -> sendBridgeTest e v >> pure True))
  ) ev e

midBridgeTest :: (Show a) => Html () -> (Event Value -> Engine -> IO a) -> Application -> Application
midBridgeTest init eeio = start $ \ ev e -> do
  append e "inputs" (Lazy.toStrict $ renderText init)
  final <- eeio ev e `E.finally` putStrLn ("midBridgeTest finalled" :: Text)
  putStrLn $ ("final value was: " :: Text) <> show final

repTest :: (Monad m) => SharedRep m (Int, Text)
repTest = do
  n <- sliderI_ "slider" 0 5 1 3
  t <- textbox_ "textbox" "sometext"
  pure (n, t)

repTest' :: (Monad m) => SharedRep m (Maybe (Int, Text))
repTest' = maybeRep "testMaybe" "wrapper" True repTest

midRepTest :: (Show a) => SharedRep IO a -> (a -> Text) -> Application -> Application
midRepTest s rend = start $ \ ev e -> do
  (Rep h fa, (_, hm)) <- flip runStateT (0,empty) (unrep s)
  append e "inputs" (Lazy.toStrict $ renderText h)
  final <- updateMap (logResults rend) hm (either Left fa) ev e
    `E.finally` putStrLn ("midTestFinalled finalled" :: Text)
  putStrLn $ ("final value was: " :: Text) <> show final

results :: (a -> Text) -> Engine -> a -> IO ()
results r e x = replace e "results" (r x)

logResults :: (a -> Text) -> Engine -> Either Text a -> IO ()
logResults _ e (Left err) = append e "log" err
logResults r e (Right x) = results r e x

data RepExamples =
  RepExamples
  { repTextbox :: Text
  , repSliderI :: Int
  , repSlider :: Double
  , repCheckbox :: Bool
  , repToggle :: Bool
  , repButton :: ()
  , repDropdown :: Int
  , repColor :: PixelRGB8
  } deriving (Show, Eq)

repExamples :: (Monad m) => SharedRep m RepExamples
repExamples = do
  t <- textbox_ "textbox" "sometext"
  n <- sliderI_ "int slider" 0 5 1 3
  ds <- slider_ "double slider" 0 1 0.1 0.5
  c <- checkbox_ "checkbox" True
  tog <- toggle_ "toggle" False
  b <- button_ "button"
  dr <- dropdown_ decimal show "dropdown" (show <$> [1..5]) 3
  col <- color_ "color" (PixelRGB8 56 128 200)
  pure (RepExamples t n ds c tog b dr col)

data Opts w = Opts
  { log :: w ::: Maybe Bool <?> "server log to stdout"
  , logPath :: w ::: Maybe Bool <?> "log raw path"
  , bridgeOnly :: w ::: Maybe Bool <?> "bridge-only test (no rep test)"
  } deriving (Generic)

instance ParseRecord (Opts Wrapped)

main :: IO ()
main = do
  o :: Opts Unwrapped <- unwrapRecord "examples for web-page"
  ah <- flip evalStateT 0 (accordion "acctest" Nothing $ (\x -> (show x, "filler")) <$> [1..3::Int])
  let tr = maybe False id
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "other")
    when (tr $ log o) $
      middleware logStdoutDev
    when (tr $ logPath o) $
      middleware $ \app req res ->
        putStrLn ("raw path:" :: Text) >>
        print (rawPathInfo req) >> app req res
  -- Only one middleware servicing the web socket can be run at a time.  Simply switching on based on paths doesn't work because socket comms comes through "/"
  -- so that the first bridge middleware consumes all the elements
    middleware $ bool
        (midRepTest
          (maybeRep "wrapped examples" "wrapper" True repExamples) show)
        (midBridgeTest (toHtml rangeTest <> toHtml textTest)
          consumeBridgeTest)
        (tr $ bridgeOnly o)
    servePageWith "/simple" defaultPageConfig page1
    servePageWith "/accordion" defaultPageConfig (testPage ah)
    servePageWith "/rep" defaultPageConfig
      (ioTestPage mempty (bool mempty (toHtml (show initBridgeTest :: Text)) (tr $ bridgeOnly o)))
