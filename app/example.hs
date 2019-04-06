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

import Control.Lens
import Data.Aeson (Value)
import Data.Attoparsec.Text
import Data.HashMap.Strict
import Lucid hiding (b_)
import Network.JavaScript
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Protolude hiding (replace, empty, Rep)
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
  n <- slider_ "slider" 0 5 3
  t <- textbox_ "textbox" "sometext"
  pure (n, t)

repTest' :: (Monad m) => SharedRep m (Maybe (Int, Text))
repTest' = maybeRep "testMaybe" "wrapper" True repTest

midRepTest :: (Show a) => SharedRep IO a -> (a -> Text) -> Application -> Application
midRepTest s rend = start $ \ ev e -> do
  (Rep h fa, (_, hm)) <- flip runStateT (0,empty) (unrep s)
  append e "inputs" (Lazy.toStrict $ renderText h)
  final <- consumeSharedBridge (logResults rend) hm (either Left fa) ev e
    `E.finally` putStrLn ("scootMidTest finalled" :: Text)
  putStrLn $ ("final value was: " :: Text) <> show final

results :: (a -> Text) -> Engine -> a -> IO ()
results r e x = replace e "results" (r x)

logResults :: (a -> Text) -> Engine -> Either Text a -> IO ()
logResults _ e (Left err) = append e "log" err
logResults r e (Right x) = results r e x

-- FIXME: work out how to leave middleware all uncommented
-- Simply switching on based on paths doesn't work because socket comms comes through "/"
-- so the first bridge middleware consumes all the elements
main :: IO ()
main = do
  ah <- flip evalStateT 0 (accordion "acctest" Nothing $ (\x -> (show x, "filler")) <$> [1..3::Int])
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "other")
    -- middleware logStdoutDev
    -- middleware $ \app req res -> putStrLn ("raw path:" :: Text) >> print (rawPathInfo req) >> app req res
    let inputBridgeTest = toHtml rangeTest <> toHtml textTest
    middleware (midBridgeTest inputBridgeTest consumeBridgeTest)
    servePageWith "/bridge" defaultPageConfig
      (ioTestPage mempty (toHtml (show initBridgeTest :: Text)))
    servePageWith "/default" defaultPageConfig page1
    servePageWith "/accordion" defaultPageConfig (testPage ah)
    -- /rep will not work until you comment out /bridge
    middleware (midRepTest repTest' show)
    servePageWith "/rep" defaultPageConfig
      (ioTestPage mempty mempty)


