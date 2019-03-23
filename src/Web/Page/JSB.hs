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

import Web.Page.Types
import Web.Page.Js
import Protolude hiding (replace)
import Text.InterpolatedString.Perl6
import Lens.Micro
import Data.Aeson
import Data.Aeson.Types
import Network.JavaScript
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy

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

{-
echo :: Value -> IO ()
echo v = void $ with (cStdout 100) $ \c -> atomically (commit c (show (fromJSON v :: Result Value) :: Text))
-}

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


