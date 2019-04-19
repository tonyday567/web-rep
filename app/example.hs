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
import Protolude hiding (replace, Rep, log, empty)
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

repConcerns :: (Monad m) => Concerns Text -> SharedRep m (Concerns Text)
repConcerns (Concerns c j h) = do
  hrep <- textarea 10 "html" h
  crep <- textarea 10 "css" c
  jrep <- textarea 10 "js" j
  pure $ Concerns crep jrep hrep

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
  ) (bridge ev e)

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
  (bridge ev e)

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
  (bridge ev e)

results :: (a -> Text) -> Engine -> a -> IO ()
results r e x = replace e "results" (r x)

logResults :: (a -> Text) -> Engine -> Either Text a -> IO ()
logResults _ e (Left err) = append e "log" (err <> "<br>")
logResults r e (Right x) = results r e x

logConcerns :: (Concerns Text -> Text) -> Engine -> Either Text (Concerns Text) -> IO ()
logConcerns _ e (Left err) = append e "log" err
logConcerns _ e (Right x) = sendConcerns e "results" x

data MidType = Dev | Prod | Debug | Bridge | Fiddle | NoMid deriving (Eq, Read, Show, Generic)

instance ParseField MidType
instance ParseRecord MidType
instance ParseFields MidType

data Opts w = Opts
  { midtype :: w ::: MidType <?> "type of middleware processing"
  , log :: w ::: Maybe Bool <?> "server log to stdout"
  , logPath :: w ::: Maybe Bool <?> "log raw path"
  } deriving (Generic)

instance ParseRecord (Opts Wrapped)

main :: IO ()
main = do
  o :: Opts Unwrapped <- unwrapRecord "examples for web-page"
  (Rep ah _) <- flip evalStateT (0, empty) $ unrep devlist
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
    middleware $ case midtype o of
      NoMid -> id
      Prod -> midRunShared
          (maybeRep "maybe" True repExamples) (logResults show)
      Debug -> midRunShared
          (maybeRep "maybe" True repExamples) (logResults show)
      Dev -> midRunShared
          -- ((,) <$> button "button" <*> listifyExample) (logResults show)
          (repSumTypeExample 2 "text" (SumInt 11))
          (logResults show)
      Bridge -> midBridgeTest (toHtml rangeTest <> toHtml textTest)
           consumeBridgeTest
      Fiddle -> midEvalShared
          (repConcerns (fiddleExampleDev 5))
          (logConcerns show)
    servePageWith "/simple" defaultPageConfig page1
    servePageWith "/fiddle" defaultPageConfig (ioTestPage mempty mempty)
    servePageWith "/accordion" defaultPageConfig (testPage ah)
    servePageWith "/rep" defaultPageConfig
      (ioTestPage mempty (bool mempty
                          (toHtml (show initBridgeTest :: Text))
                          (midtype o == Bridge))) 

devlist :: (Monad m) => SharedRep m [Int]
devlist = accordionListify (Just "accordionListify example") "prefix" (Just "[2]") (\l a -> sliderI l (0::Int) 10 1 a) ((\x -> "[" <> show x <> "]") <$> [0..10::Int] :: [Text]) [0..10]



