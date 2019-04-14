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
import Lucid hiding (b_)
import Network.JavaScript
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Network.Wai.Middleware.RequestLogger
import Protolude hiding (replace, Rep, log)
import Web.Page
import Web.Page.Bootstrap
import Web.Page.Examples
import Web.Page.Html.Input
import Web.Page.Bridge
import Web.Page.Rep
import Web.Scotty hiding (get, put)
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
  execValueConsume initBridgeTest stepBridgeTest'
  ( (Box.liftC <$> Box.showStdout) <>
    pure (Box.Committer (\v -> sendBridgeTest e v >> pure True))
  ) ev

midBridgeTest :: (Show a) => Html () -> (Event Value -> Engine -> IO a) -> Application -> Application
midBridgeTest init eeio = start $ \ ev e -> do
  append e "inputs" (Lazy.toStrict $ renderText init)
  final <- eeio ev e `E.finally` putStrLn ("midBridgeTest finalled" :: Text)
  putStrLn $ ("final value was: " :: Text) <> show final

initShared
  :: (MonadIO m)
  => MonadState (HashMap Text Text) m
  => Html ()
  -> (HashMap Text Text -> (HashMap Text Text, Either Text a))
  -> ((HashMap Text Text, Either Text a) -> Text)
  -> Engine
  -> m ()
initShared h fa f e = do
  liftIO $ append e "inputs" (Lazy.toStrict $ renderText h)
  hm0 <- get
  let (hm1, r) = fa hm0
  put hm1
  liftIO $ replace e "results" (f (hm1,r))

-- | evaluate the ShareRep a, and act
midEvalShared ::
  (Show a) =>
  SharedRep IO a ->
  (Engine -> Either Text a -> IO ()) ->
  Application -> Application
midEvalShared s action = start $ \ ev e ->
  evalSharedRepOnEvent
  s
  (\h fa -> zoom _2 $ initShared h fa (show . snd) e)
  (do
      f <- get
      liftIO $ putStrLn $ ("final value was: " :: Text) <> show f)
  (action e)
  ev

-- | evaluate the ShareRep a, and act on the results and state
midRunShared ::
  (Show a) =>
  SharedRep IO a ->
  (Engine -> Either Text (HashMap Text Text, Either Text a) -> IO ()) ->
  Application -> Application
midRunShared s action = start $ \ ev e ->
  runSharedRepOnEvent
  s
  (\h fa -> zoom _2 $ initShared h fa show e)
  (do
      f <- get
      liftIO $ putStrLn $ ("final value was: " :: Text) <> show f)
  (action e)
  ev

results :: (a -> Text) -> Engine -> a -> IO ()
results r e x = replace e "results" (r x)

logResults :: (a -> Text) -> Engine -> Either Text a -> IO ()
logResults _ e (Left err) = append e "log" err
logResults r e (Right x) = results r e x

data Opts w = Opts
  { log :: w ::: Maybe Bool <?> "server log to stdout"
  , logPath :: w ::: Maybe Bool <?> "log raw path"
  , bridgeOnly :: w ::: Maybe Bool <?> "bridge-only test (no rep test)"
  , dev :: w ::: Maybe Bool <?> "rando development"
  , debug :: w ::: Maybe Bool <?> "debug mode"
  , record :: w ::: Maybe Bool <?> "record events and state"
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
    middleware $ case (tr $ bridgeOnly o, tr $ debug o, tr $ dev o, tr $ record o) of
      (_, _, _, True) -> midRunShared
          (maybeRep "maybe" True repExamples) (logResults show)
      (_, True, _, _) -> midRunShared
          (maybeRep "maybe" True repExamples) (logResults show)
      (_, _, True, _) -> midRunShared
          -- ((,) <$> button "button" <*> listifyExample) (logResults show)
          ((,) <$> buttonB "button1" <*> buttonB "button2")
          (logResults show)
      (False, _, _, _) -> midEvalShared
          (maybeRep "maybe" True repExamples) (logResults show)
      _ -> midBridgeTest (toHtml rangeTest <> toHtml textTest)
           consumeBridgeTest
    servePageWith "/simple" defaultPageConfig page1
    servePageWith "/accordion" defaultPageConfig (testPage ah)
    servePageWith "/rep" defaultPageConfig
      (ioTestPage mempty (bool mempty (toHtml (show initBridgeTest :: Text)) (tr $ bridgeOnly o)))
