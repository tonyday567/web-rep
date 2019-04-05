{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

module Web.Page.JSB where

import Box
import Control.Monad.Morph
import Data.Aeson
import Data.Aeson.Types
import Lens.Micro
import Lucid
import Network.JavaScript
import Protolude hiding (replace)
import Text.InterpolatedString.Perl6
import Web.Page.Js
import Web.Page.Types
import qualified Control.Exception as E
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Streaming.Prelude as S

jsbPreventEnter :: PageJs
jsbPreventEnter = PageJs $ fromText [q|
window.addEventListener('keydown',function(e) {
  if(e.keyIdentifier=='U+000A' || e.keyIdentifier=='Enter' || e.keyCode==13) {
    if(e.target.nodeName=='INPUT' && e.target.type !== 'textarea') {
      e.preventDefault();
      return false;
    }
  }
}, true);
|]

jsbWebSocket :: PageJs
jsbWebSocket = PageJsText [q|
window.jsb = {ws: new WebSocket('ws://' + location.host + '/')};
jsb.ws.onmessage = (evt) => eval(evt.data);
|]

jsbPage :: Page
jsbPage =
  mempty &
  #jsGlobal .~ jsbPreventEnter &
  #jsOnLoad .~ jsbWebSocket


result :: Engine -> Value -> IO ()
result e v =
  either
  (append e "log" . Text.pack)
  (replace e "results" . (show :: Element -> Text))
  (parseEither parseJSON v)

replace :: Engine -> Text -> Text -> IO ()
replace e d t = send e $ command $ Lazy.fromStrict $ "document.getElementById('" <> d <> "').innerHTML = '" <> t <> "'"

append :: Engine -> Text -> Text -> IO ()
append e d t = send e $ command $ Lazy.fromStrict $ "document.getElementById('" <> d <> "').innerHTML += '" <> t <> "'"

-- | Change messages that come from the JS side. Invariably, this is Text, and we choose to type convert on the haskell side
data Element = Element
  { element :: Text
  , value :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Element

instance FromJSON Element
  where
    parseJSON = withObject "Element" $ \v ->
      Element <$>
      v .: "element" <*>
      v .: "value"

jsbIncoming :: Cont IO (Committer IO Value) -> Int -> Event Value -> Engine -> IO ()
jsbIncoming ccont timeout ev e = do
  putStrLn ("jsbIncoming" :: Text)
  append e "log" "jsbIncoming up ..."
  _ <- addListener ev (\v -> void $ Box.with ccont $ \c -> commit c v)
  threadDelay (timeout * 1000 * 1000)
  replace e "log" "jsbIncoming timed out"

eventCIO :: Engine -> Event Value -> Committer IO Value -> IO ()
eventCIO _ ev c = void $ addListener ev (void . commit c)

eventCIOLog :: Engine -> Event Value -> Committer IO Value -> IO ()
eventCIOLog _ ev c = do
  putStrLn ("eventCIO" :: Text)
  void $ addListener ev
    (\v -> do
        putStrLn ("eventCIO fired: " <> (show v :: Text))
        void $ commit c v)

jsbEIO :: Engine -> Event Value -> Cont IO (Emitter IO Value)
jsbEIO e ev = Cont $
  \eio -> queueELog (eventCIO e ev) eio

transEcho :: Transducer Int Value Text
transEcho = Transducer $ \s -> s & S.map (("echo: " <>) . show)

etcEcho :: Cont IO (Emitter IO Value) -> IO Int
etcEcho e = etcM 0 transEcho (Box <$> (liftC <$> cStdout 10) <*> e)

jsbQ :: Event Value -> Engine -> IO ()
jsbQ ev e = do
  join $ print <$> etcEcho (jsbEIO e ev)
  replace e "log" "jsbQ thread completed"

jsbEcho' :: Event Value -> Engine -> IO ()
jsbEcho' ev e = do
  Box.with (jsbEIO e ev) (eStdoutM 10 . fmap (show :: Value -> Text))
  replace e "log" "jsbEcho' thread completed"

echo :: Value -> IO ()
echo v = void $ Box.with (cStdout 100) $ \c -> atomically (commit c (show (fromJSON v :: Result Value) :: Text))

eventCont :: Cont IO (Committer STM Value) -> Engine -> Event Value -> IO ()
eventCont cc _ ev = void $ addListener ev (\v -> void $ Box.with cc $ \c -> atomically (commit c v))

eventBox :: Cont IO (Committer IO (Either Text Element)) -> Event Value -> Engine -> IO ()
eventBox comm ev _ = do
  (c,e) <- atomically $ ends Unbounded
  void $ addListener ev (atomically . c)
  n <- etcM 0 model
    (Box <$>
     comm <*>
     (liftE <$> pure (Emitter (Just <$> e))))
  print n

toEither :: Result a -> Either Text a
toEither (Success a) = Right a
toEither (Error e) = Left $ Text.pack e

model :: Transducer Int Value (Either Text Element)
model = Transducer $ \s -> s & S.map (toEither . fromJSON) & S.chain (\_ -> modify (+1))

outputTest :: Engine -> Either Text Element -> IO ()
outputTest e (Left err) = append e "log" err
outputTest e (Right (Element i v)) =
  replace e "results" (i <> ":" <> show v)

eventOutputTest :: Event Value -> Engine -> IO ()
eventOutputTest ev e =
  eventBox
  ( (liftC <$> showStdout) <>
    pure (Committer (\v -> outputTest e v >> pure True))
  ) ev e

jsbMid :: Html () -> (Event Value -> Engine -> IO ()) -> Application -> Application
jsbMid h eeio = start $ \ ev e -> do
  append e "inputs" (Lazy.toStrict $ renderText h)
  eeio ev e `E.finally` putStrLn ("jsbMid finalled" :: Text)

model' :: (FromJSON a, MonadState s m) => (a -> s -> s) -> S.Stream (S.Of Value) m () -> S.Stream (S.Of (Either Text s)) m ()
model' step s =
  s &
  S.map (toEither . fromJSON) &
  S.partitionEithers &
  hoist (S.chain (modify . step)) &
  hoist (S.mapM (\_ -> get)) &
  S.unseparate &
  S.maps S.sumToEither

-- | eventConsume
eventConsume :: s -> (Element -> s -> s) -> Cont IO (Committer IO (Either Text s)) -> Event Value -> Engine -> IO s
eventConsume init step comm ev _ = do
  (c,e) <- atomically $ ends Unbounded
  void $ addListener ev (atomically . c)
  final <- etcM init (Transducer (model' step))
    (Box <$> comm <*> (liftE <$> pure (Emitter (Just <$> e))))
  pure final
