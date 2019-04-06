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

module Web.Page.Bridge
  ( Element(..)
  , bridgePage
  , append
  , replace
  , eventConsume
  ) where

import Box
import Control.Monad.Morph
import Data.Aeson
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

bridgePage :: Page
bridgePage =
  mempty &
  #jsGlobal .~ jsbPreventEnter &
  #jsOnLoad .~ jsbWebSocket

replace :: Engine -> Text -> Text -> IO ()
replace e d t = send e $ command $ Lazy.fromStrict $ "document.getElementById('" <> d <> "').innerHTML = '" <> t <> "'"

append :: Engine -> Text -> Text -> IO ()
append e d t = send e $ command $ Lazy.fromStrict $ "document.getElementById('" <> d <> "').innerHTML += '" <> t <> "'"

-- | messages that come from the Bridge. Invariably, this is JSON/Text, and we choose to type convert on the haskell side
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

eventCIO :: Engine -> Event Value -> Committer IO Value -> IO ()
eventCIO _ ev c = void $ addListener ev (void . commit c)

eventEIO :: Engine -> Event Value -> Cont IO (Emitter IO Value)
eventEIO e ev = Cont $
  \eio -> queueELog (eventCIO e ev) eio

transEcho :: Transducer Int Value Text
transEcho = Transducer $ \s -> s & S.map (("echo: " <>) . show)

etcEcho :: Cont IO (Emitter IO Value) -> IO Int
etcEcho e = etcM 0 transEcho (Box <$> (liftC <$> cStdout 10) <*> e)

eventQueue :: Event Value -> Engine -> IO ()
eventQueue ev e = do
  join $ print <$> etcEcho (eventEIO e ev)
  replace e "log" "event queue thread completed"

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

bridgeMid :: Html () -> (Event Value -> Engine -> IO ()) -> Application -> Application
bridgeMid h eeio = start $ \ ev e -> do
  append e "inputs" (Lazy.toStrict $ renderText h)
  eeio ev e `E.finally` putStrLn ("bridgeMid finalled" :: Text)

eventModel :: (FromJSON a, MonadState s m) => (a -> s -> s) -> S.Stream (S.Of Value) m () -> S.Stream (S.Of (Either Text s)) m ()
eventModel step s =
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
  final <- etcM init (Transducer (eventModel step))
    (Box <$> comm <*> (liftE <$> pure (Emitter (Just <$> e))))
  pure final
