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
import Protolude hiding ((<<*>>), replace, Rep, log)
import Web.Page
import Web.Page.Bootstrap
import Web.Page.Examples
import Web.Page.Html
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
import Data.Biapplicative

ioTestPage :: Text -> Text -> Html () -> Html () -> Page
ioTestPage title mid i r =
  showJs <>
  bootstrapPage <>
  bridgePage &
  #htmlHeader .~ title_ "ioTestPage" &
  #htmlBody .~ b_ "container"
  (mconcat
    [ b_ "row" (h1_ (toHtml title))
    , b_ "row" (h2_ ("middleware: " <> toHtml mid))
    , b_ "row" (b_ "col-sm" (h2_ "input" <> with form_ [id_ "input"] i) <>
           b_ "col-sm" (with div_ [id_ "output"]
                  (h2_ "output" <>
                   with div_ [id_ "results"] r)))
    , b_ "row" (with div_ [id_ "log"] (h2_ "server log"))
    ])

iroTestPage :: Text -> Text -> Html () -> Html () -> Html () -> Page
iroTestPage title mid input representation output =
  showJs <>
  bootstrapPage <>
  bridgePage &
  #htmlHeader .~ title_ "iroTestPage" &
  #htmlBody .~ b_ "container"
  (mconcat
    [ b_ "row" (h1_ (toHtml title))
    , b_ "row" (h2_ ("middleware: " <> toHtml mid))
    , b_ "row" $ mconcat
      [ b_ "col" (h2_ "input" <> with div_ [id_ "input"] input)
      , b_ "col" $ mconcat
        [ b_ "col" (h2_ "representation" <> with div_ [id_ "representation"] representation)
        , b_ "col" (h2_ "output" <> with div_ [id_ "output"] output)
        , b_ "col" (h2_ "log" <> with div_ [id_ "log"] mempty)
        ]
      ]
    ])

-- | bridge testing without the SharedRep method
rangeTest :: Input Int
rangeTest = bridgeify $ bootify $
  Input
  3
  Slider
  (Just "range example")
  Nothing
  "rangeid"
  [ style_ "max-width:15rem;"
  , min_ "0"
  , max_ "5"
  , step_ "1"
  ]

textTest :: Input Text
textTest = bridgeify $ bootify $
  Input
  "abc"
  TextBox
  (Just "label")
  Nothing
  "textid"
  [ style_ "max-width:15rem;"
  , placeholder_ "test placeholder"
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
  append e "input" (Lazy.toStrict $ renderText init)
  final <- eeio ev e `E.finally` putStrLn ("midBridgeTest finalled" :: Text)
  putStrLn $ ("final value was: " :: Text) <> show final

-- * SharedRep testing
initShared
  :: (MonadIO m)
  => MonadState (HashMap Text Text) m
  => Html ()
  -> (HashMap Text Text -> (HashMap Text Text, Either Text a))
  -> ((HashMap Text Text, Either Text a) -> Text)
  -> Engine
  -> m ()
initShared h fa f e = do
  liftIO $ append e "input" (Lazy.toStrict $ renderText h)
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

-- | evaluate a Fiddle, without attempting to downstream bridging
midFiddle ::
  Concerns Text ->
  Application -> Application
midFiddle cs = start $ \ ev e ->
  evalSharedRepOnEvent
  (repConcerns cs)
  (\h _ ->
      liftIO $ append e "input" (toText h))
  (do
      f <- get
      liftIO $ putStrLn $ ("final value was: " :: Text) <> show f)
  (logFiddle_ e)
  (bridge ev e)

logFiddle_ :: Engine -> Either Text (Concerns Text, Bool) -> IO ()
logFiddle_ e (Left err) = append e "log" err
logFiddle_ e (Right (c,u)) = bool (pure ()) (sendConcerns e "output" c) u

repConcerns :: (Monad m) => Concerns Text -> SharedRep m (Concerns Text, Bool)
repConcerns (Concerns c j h) =
  bimap
  (\c' j' h' up -> (with div_ [class__ "fiddle "] $ mconcat [up,h',j',c']))
  (\c' j' h' up -> (Concerns c' j' h', up))
  (textarea 10 "css" c) <<*>>
  textarea 10 "js" j <<*>>
  textarea 10 "html" h <<*>>
  buttonB "update"

viaFiddle
  :: (Monad m)
  => SharedRep m a
  -> SharedRep m (Bool, Concerns Text, a)
viaFiddle sr = SharedRep $ do
  sr'@(Rep h _) <- unrep sr
  hrep <- unrep $ textarea 10 "html" (Lazy.toStrict $ renderText h)
  crep <- unrep $ textarea 10 "css" mempty
  jrep <- unrep $ textarea 10 "js" mempty
  u <- unrep $ buttonB "update"
  pure $
    bimap
    (\up a b c _ -> (with div_ [class__ "fiddle "] $ mconcat [a, b, c, up]))
    (\up a b c d -> (up, Concerns a b c, d))
    u <<*>>
    crep <<*>>
    jrep <<*>>
    hrep <<*>>
    sr'

initFiddle
  :: (MonadIO m)
  => MonadState (HashMap Text Text) m
  => Html ()
  -> (HashMap Text Text -> (HashMap Text Text, Either Text (Bool, Concerns Text, a)))
  -> (a -> Text)
  -> Engine
  -> m ()
initFiddle h fa showa e = do
  liftIO $ append e "input" (Lazy.toStrict $ renderText h)
  hm0 <- get
  let (hm1, r) = fa hm0
  put hm1
  liftIO $ case r of
    Left err -> append e "log" err
    Right (_,c,a) -> do
      sendConcerns e "representation" c
      replace e "output" (showa a)

midViaFiddle ::
  (Show a) =>
  SharedRep IO (Bool, Concerns Text, a) ->
  (Engine -> Either Text (Bool, Concerns Text, a) -> IO ()) ->
  Application -> Application
midViaFiddle s action = start $ \ ev e ->
  evalSharedRepOnEventF
  s
  (\h fa ->
      zoom _2 $ initFiddle h fa show e)
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

logFiddle :: (a -> Text) -> Engine -> Either Text (Bool, Concerns Text, a) -> IO ()
logFiddle _ e (Left err) = append e "log" err
logFiddle r e (Right (u,c,a)) =
  case u of
    True ->
      sendConcerns e "representation" c
    False -> do
      putStrLn (r a)
      replace e "output" (r a)

data MidType = Dev | Prod | Bridge | Listify | Fiddle | ViaFiddle | NoMid deriving (Eq, Read, Show, Generic)

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
      Dev -> midRunShared
          (maybeRep "maybe" True repExamples) (logResults show)
      Listify -> midRunShared listifyExample (logResults show)
      Bridge -> midBridgeTest (toHtml rangeTest <> toHtml textTest)
           consumeBridgeTest
      Fiddle -> midFiddle fiddleExample
      ViaFiddle -> midViaFiddle
          (viaFiddle $ repSumTypeExample 2 "default text" SumOnly)
          (logFiddle show)
    servePageWith "/simple" defaultPageConfig page1
    servePageWith "/iro" defaultPageConfig (iroTestPage "iro" (show $ midtype o) mempty mempty mempty)
    servePageWith "/" defaultPageConfig
      (ioTestPage "prod" (show $ midtype o) mempty (bool mempty
                          (toHtml (show initBridgeTest :: Text))
                          (midtype o == Bridge))) 



 
