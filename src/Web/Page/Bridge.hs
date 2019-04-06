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
  , elementConsume
  ) where

import Box
import Control.Monad.Morph
import Data.Aeson
import Control.Lens
import Network.JavaScript
import Protolude hiding (replace)
import Text.InterpolatedString.Perl6
import Web.Page.Js
import Web.Page.Types
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

toEither :: Result a -> Either Text a
toEither (Success a) = Right a
toEither (Error e) = Left $ Text.pack e

elementModel :: (FromJSON a, MonadState s m) => (a -> s -> s) -> S.Stream (S.Of Value) m () -> S.Stream (S.Of (Either Text s)) m ()
elementModel step s =
  s &
  S.map (toEither . fromJSON) &
  S.partitionEithers &
  hoist (S.chain (modify . step)) &
  hoist (S.mapM (\_ -> get)) &
  S.unseparate &
  S.maps S.sumToEither

-- | eventConsume
elementConsume :: s -> (Element -> s -> s) -> Cont IO (Committer IO (Either Text s)) -> Event Value -> Engine -> IO s
elementConsume init step comm ev _ = do
  (c,e) <- atomically $ ends Unbounded
  void $ addListener ev (atomically . c)
  final <- etcM init (Transducer (elementModel step))
    (Box <$> comm <*> (liftE <$> pure (Emitter (Just <$> e))))
  pure final
