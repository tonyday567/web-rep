{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

module Web.Page.Bridge
  ( bridgePage
  , sendc
  , append
  , replace
  , bridge
  , sendConcerns
  ) where

import Control.Lens
import Network.JavaScript
import Protolude hiding (replace)
import Text.InterpolatedString.Perl6
import Web.Page.Js
import Web.Page.Types
import qualified Data.Text.Lazy as Lazy
import Data.Aeson (Value)
import Box.Cont
import Lucid
import qualified Data.Text as Text

preventEnter :: PageJs
preventEnter = PageJs $ fromText [q|
window.addEventListener('keydown',function(e) {
  if(e.keyIdentifier=='U+000A' || e.keyIdentifier=='Enter' || e.keyCode==13) {
    if(e.target.nodeName=='INPUT' && e.target.type !== 'textarea') {
      e.preventDefault();
      return false;
    }
  }
}, true);
|]

webSocket :: PageJs
webSocket = PageJsText [q|
window.jsb = {ws: new WebSocket('ws://' + location.host + '/')};
jsb.ws.onmessage = (evt) => eval(evt.data);
|]

bridgePage :: Page
bridgePage =
  mempty &
  #jsGlobal .~ preventEnter &
  #jsOnLoad .~ webSocket

sendc :: Engine -> Text -> IO ()
sendc e = send e . command . Lazy.fromStrict

replace :: Engine -> Text -> Text -> IO ()
replace e d t = send e $ command $ Lazy.fromStrict $ "document.getElementById('" <> d <> "').innerHTML = '" <> clean t <> "'"

append :: Engine -> Text -> Text -> IO ()
append e d t = send e $ command $ Lazy.fromStrict $ "document.getElementById('" <> d <> "').innerHTML += '" <> clean t <> "'"

sendConcerns :: Engine -> Text -> Concerns Text -> IO ()
sendConcerns e t (Concerns c j h) = do
  replace e t h
  append e t (Lazy.toStrict $ renderText $ style_ c)
  sendc e j

bridge :: Event Value -> Engine -> Cont_ IO Value
bridge ev _ = Cont_ $ \vio -> void $ addListener ev vio

clean :: Text -> Text
clean =
  Text.intercalate "\\'" . Text.split (=='\'') .
  Text.intercalate "\\n" . Text.lines
