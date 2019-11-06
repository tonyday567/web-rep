{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}

module Web.Page.Bridge
  ( bridgePage
  , sendc
  , append
  , replace
  , appendWithScript
  , replaceWithScript
  , bridge
  , sendConcerns
  , Engine
  , start
  , Application
  , midShared
  ) where

import Box.Cont
import Control.Lens
import Data.Aeson (Value)
import Data.HashMap.Strict as HashMap
import Lucid
import Network.JavaScript (Engine, start, send, command, addListener, JavaScript(..), Application)
import Protolude hiding (replace, Rep)
import Text.InterpolatedString.Perl6
import Web.Page.Html
import Web.Page.Js
import Web.Page.Rep
import Web.Page.Types
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

-- see https://ghinda.net/article/script-tags/
runScriptJs :: PageJs
runScriptJs = PageJsText [q|

function insertScript ($script) {
  var s = document.createElement('script')
  s.type = 'text/javascript'
  if ($script.src) {
    s.onload = callback
    s.onerror = callback
    s.src = $script.src
  } else {
    s.textContent = $script.innerText
  }

  // re-insert the script tag so it executes.
  document.head.appendChild(s)

  // clean-up
  $script.parentNode.removeChild($script)
}

function runScripts ($container) {
  // get scripts tags from a node
  var $scripts = $container.querySelectorAll('script')
  $scripts.forEach(function ($script) {
    insertScript($script)
  })
}
|]

bridgePage :: Page
bridgePage =
  mempty &
  #jsGlobal .~ (preventEnter <> runScriptJs) &
  #jsOnLoad .~ webSocket

sendc :: Engine -> Text -> IO ()
sendc e = send e . command . JavaScript . fromStrict

replace :: Engine -> Text -> Text -> IO ()
replace e d t = send e $ command
  [qc|
     var $container = document.getElementById('{d}')
     $container.innerHTML = '{clean t}'
     runScripts($container)
     |]

append :: Engine -> Text -> Text -> IO ()
append e d t = send e $ command
    [qc|
     var $container = document.getElementById('{d}')
     $container.innerHTML += '{clean t}'
     runScripts($container)
     |]

replaceWithScript :: Engine -> Text -> Text -> IO ()
replaceWithScript e d t = send e $ command
  [qc|
     var $container = document.getElementById('{d}')
     $container.innerHTML = '{clean t}'
     runScripts($container)
     |]

appendWithScript :: Engine -> Text -> Text -> IO ()
appendWithScript e d t = send e $ command
    [qc|
     var $container = document.getElementById('{d}')
     $container.innerHTML += '{clean t}'
     runScripts($container)
     |]

sendConcerns :: Engine -> Text -> Concerns Text -> IO ()
sendConcerns e t (Concerns c j h) = do
  replaceWithScript e t h
  append e t (toText $ style_ c)
  sendc e j

bridge :: Engine -> Cont_ IO Value
bridge e = Cont_ $ \vio -> void $ addListener e vio

clean :: Text -> Text
clean =
  Text.intercalate "\\'" . Text.split (=='\'') .
  Text.intercalate "\\n" . Text.lines


-- | create Wai Middleware for a SharedRep providing an initialiser and action on events
midShared ::
  (Show a) =>
  SharedRep IO a ->
  (Engine -> Rep a -> StateT (HashMap Text Text) IO ()) ->
  (Engine -> Either Text (HashMap Text Text, Either Text a) -> IO ()) ->
  Application -> Application
midShared sr init action = start $ \e ->
  void $ runOnEvent
  sr
  (zoom _2 . init e)
  (action e)
  (bridge e)




