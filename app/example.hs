{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

import qualified Box
import Control.Lens hiding (Unwrapped, Wrapped)
import Data.Attoparsec.Text (decimal, parseOnly)
import Lucid
import Network.Wai.Middleware.Static ((>->), addBase, noDots, staticPolicy)
import NumHask.Prelude hiding (log, replace)
import Options.Generic
import Web.Page
import Web.Page.Examples
import Web.Scotty (middleware, scotty)

testPage :: Text -> Text -> [(Text, Html ())] -> Page
testPage title mid sections =
  bootstrapPage
    <> bridgePage
    & #htmlHeader .~ title_ "iroTestPage"
    & #htmlBody
      .~ divClass_
        "container"
        ( mconcat
            [ divClass_ "row" (h1_ (toHtml title)),
              divClass_ "row" (h2_ ("middleware: " <> toHtml mid)),
              divClass_ "row" $ mconcat $ (\(t, h) -> divClass_ "col" (h2_ (toHtml t) <> with div_ [id_ t] h)) <$> sections
            ]
        )

-- | bridge testing without the SharedRep method
rangeTest :: Input Int
rangeTest =
  Input
    3
    (Just "range example")
    "rangeid"
    ( Slider
        [ style_ "max-width:15rem;",
          min_ "0",
          max_ "5",
          step_ "1"
        ]
    )

textTest :: Input Text
textTest =
  Input
    "abc"
    (Just "label")
    "textid"
    TextBox

initBridgeTest :: (Int, Text)
initBridgeTest = (rangeTest ^. #inputVal, textTest ^. #inputVal)

stepBridgeTest :: Element -> (Int, Text) -> (Int, Text)
stepBridgeTest e s =
  case step e s of
    Left _ -> s
    Right x -> x
  where
    step (Element "rangeid" v) (_, t) =
      either
        (Left . pack)
        (\x -> Right (x, t))
        (parseOnly decimal v)
    step (Element "textid" v) (n, _) = Right (n, v)
    step e' _ = Left $ "unknown id: " <> pack (show e')

sendBridgeTest :: (Show a) => Engine -> Either Text a -> IO ()
sendBridgeTest e (Left err) = append e "log" err
sendBridgeTest e (Right a) =
  replace
    e
    "output"
    ( toText $
        cardify
          (mempty, [])
          (Just "output was:")
          (toHtml (show a :: Text), [])
    )

consumeBridgeTest :: Engine -> IO (Int, Text)
consumeBridgeTest e =
  valueConsume
    initBridgeTest
    stepBridgeTest
    ( (Box.liftC <$> Box.showStdout)
        <> pure (Box.Committer (\v -> sendBridgeTest e v >> pure True))
    )
    (bridge e)

midBridgeTest :: (Show a) => Html () -> (Engine -> IO a) -> Application -> Application
midBridgeTest init eeio = start $ \e -> do
  append e "input" (toText init)
  final <- eeio e `finally` putStrLn ("midBridgeTest finalled" :: Text)
  putStrLn ("final value was: " <> show final :: Text)

-- * SharedRep testing

-- | Middleware that shows the current shared values
midShow :: (Show a) => SharedRep IO a -> Application -> Application
midShow sr =
  midShared
    sr
    (initRep (pure . (: []) . pack . show))
    (updateRep (pure . (: []) . pack . show))

-- | evaluate a Fiddle, without attempting to downstream bridging
midFiddle :: Concerns Text -> Application -> Application
midFiddle cs = midShared (fiddle cs) initFiddleRep (\e -> logFiddle e . second snd)

initFiddleRep ::
  Engine ->
  Rep a ->
  StateT (HashMap Text Text) IO ()
initFiddleRep e r =
  void $
    oneRep
      r
      ( \(Rep h _) _ ->
          append e "input" (toText h)
      )

logFiddle :: Engine -> Either Text (Either Text (Concerns Text, Bool)) -> IO ()
logFiddle e (Left err) = append e "log" ("map error: " <> err)
logFiddle e (Right (Left err)) = append e "log" ("parse error: " <> err)
logFiddle e (Right (Right (c, u))) = bool (pure ()) (sendConcerns e "output" c) u

-- | evaluate a Fiddle, and any downstream bridging representation
midViaFiddle ::
  Show a =>
  SharedRep IO a ->
  Application ->
  Application
midViaFiddle sr =
  midShared (viaFiddle sr) (initViaFiddleRep (pack . show)) (\e -> logViaFiddle e (pack . show) . second snd)

initViaFiddleRep ::
  (a -> Text) ->
  Engine ->
  Rep (Bool, Concerns Text, a) ->
  StateT (HashMap Text Text) IO ()
initViaFiddleRep rend e r =
  void $
    oneRep
      r
      ( \(Rep h fa) m -> do
          append e "input" (toText h)
          case snd $ fa m of
            Left err -> append e "log" ("map error: " <> err)
            Right (_, c, a) -> do
              sendConcerns e "representation" c
              replace e "output" (rend a)
      )

logViaFiddle :: Engine -> (a -> Text) -> Either Text (Either Text (Bool, Concerns Text, a)) -> IO ()
logViaFiddle e _ (Left err) = append e "log" ("map error: " <> err)
logViaFiddle e _ (Right (Left err)) = append e "log" ("parse error: " <> err)
logViaFiddle e r (Right (Right (True, c, a))) = do
  sendConcerns e "representation" c
  replace e "output" (r a)
logViaFiddle e r (Right (Right (False, _, a))) = replace e "output" (r a)

data MidType = Dev | Prod | ChooseFileExample | DataListExample | SumTypeExample | SumType2Example | Bridge | ListExample | ListRepExample | Fiddle | ViaFiddle | NoMid deriving (Eq, Read, Show, Generic)

instance ParseField MidType

instance ParseRecord MidType

instance ParseFields MidType

data Opts w
  = Opts
      { midtype :: w ::: MidType <?> "type of middleware processing"
      }
  deriving (Generic)

instance ParseRecord (Opts Wrapped)

main :: IO ()
main = do
  o :: Opts Unwrapped <- unwrapRecord "examples for web-page"
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "other")
    middleware $ staticPolicy (noDots >-> addBase "saves")
    -- Only one middleware servicing the web socket can be run at a time.  Simply switching on based on paths doesn't work because socket comms comes through "/"
    -- so that the first bridge middleware consumes all the elements
    middleware $ case midtype o of
      NoMid -> id
      -- WebSocket connection to 'ws://localhost:3000/' failed: Error during WebSocket handshake: Unexpected response code: 200
      Prod ->
        midShow
          (maybeRep (Just "maybe") False repExamples)
      Dev ->
        midShared
          (maybeRep (Just "maybe") False repExamples)
          (initRep (pure . (: []) . pack . show))
          (updateRep (pure . (: []) . pack . show))
      ChooseFileExample ->
        midShow
          (chooseFile (Just "ChooseFile Label") "")
      DataListExample ->
        midShow
          (datalist (Just "label") ["first", "2", "3"] "2" "idlist")
      SumTypeExample ->
        midShow
          (repSumTypeExample 2 "default text" SumOnly)
      SumType2Example ->
        midShow
          (repSumType2Example 2 "default text" SumOnly (SumOutside 2))
      ListExample -> midShow (listExample 5)
      ListRepExample -> midShow (listRepExample 10)
      Bridge ->
        midBridgeTest
          (toHtml rangeTest <> toHtml textTest)
          consumeBridgeTest
      Fiddle -> midFiddle fiddleExample
      ViaFiddle ->
        midViaFiddle
          (slider Nothing 0 10 0.01 4)
    servePageWith "/simple" (defaultPageConfig "page1") page1
    servePageWith "/mathjax" (defaultPageConfig "mathjax") pagemj
    servePageWith
      "/iro"
      (defaultPageConfig "iro")
      ( testPage
          "iro"
          (pack . show $ midtype o)
          [ ("input", mempty),
            ("representation", mempty),
            ("output", mempty)
          ]
      )
    servePageWith
      "/"
      (defaultPageConfig "prod")
      ( testPage
          "prod"
          (pack . show $ midtype o)
          [ ("input", mempty),
            ( "output",
              bool
                mempty
                (toHtml (pack $ show initBridgeTest))
                (midtype o == Bridge)
            )
          ]
      )
    servePageWith
      "/log"
      (defaultPageConfig "prod")
      ( testPage
          "prod"
          (pack . show $ midtype o)
          [ ("input", mempty),
            ( "output",
              bool
                mempty
                (toHtml (show initBridgeTest :: Text))
                (midtype o == Bridge)
            ),
            ("log", mempty)
          ]
      )
