{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

-- | A socket between a web page and haskell, based on the box library.
module Web.Page.Socket
  ( socketPage,
    serveSocketBox,
    sharedServer,
    defaultSharedServer,
    OutputStep,
    SocketConfig(..),
    defaultSocketConfig,
    defaultSocketPage,
    defaultInputCode,
    defaultOutputCode,
    Code(..),
    code,
  )
where

import qualified Network.WebSockets as WS
import Box
import Box.Socket
import Control.Lens
import Control.Monad.Conc.Class as C
import Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import NumHask.Prelude hiding (intercalate, replace)
import Text.InterpolatedString.Perl6
import Web.Page.Html
import Web.Page.Server
import Web.Page.Types
import Web.Page.Bootstrap
import Web.Scotty hiding (get)
import qualified Data.Attoparsec.Text as A
import Network.Wai.Handler.WebSockets
import Lucid as L

socketPage :: Page
socketPage = mempty & #jsOnLoad .~
  mconcat
  [ webSocket,
    runScriptJs,
    refreshJsbJs,
    preventEnter
  ]

serveSocketBox :: SocketConfig -> Page -> Box IO Text Text -> IO ()
serveSocketBox cfg p b =
  scotty (cfg ^. #port) $ do
    middleware $ websocketsOr WS.defaultConnectionOptions (serverApp b)
    servePageWith "/" (defaultPageConfig "") p

type OutputStep m a =
  (HashMap Text Text -> (HashMap Text Text, Either Text a)) ->
  (a -> Text) ->
  StateT (Int, HashMap Text Text) m [Code]

sharedServer :: SharedRep IO a -> SocketConfig -> Page -> (a -> Text) -> (Html () -> [Code]) -> OutputStep IO a -> IO ()
sharedServer srep cfg p out i o =
  serveSocketBox cfg p <$.>
  fromAction (backendLoop srep out i o . wrangle)

defaultSharedServer :: (Show a) => SharedRep IO a -> IO ()
defaultSharedServer srep =
  sharedServer srep defaultSocketConfig defaultSocketPage show defaultInputCode defaultOutputCode

defaultSocketPage :: Page
defaultSocketPage =
  bootstrapPage <>
  socketPage &
  #htmlBody
      .~ divClass_
        "container"
        ( mconcat
            [ divClass_ "row" (h1_ "web-rep testing"),
              divClass_ "row" $ mconcat $ (\(t, h) -> divClass_ "col" (h2_ (toHtml t) <> L.with div_ [id_ t] h)) <$> sections
            ]
        )
  where
    sections = 
      [ ("input", mempty),
        ("output", mempty)
      ]

-- I am proud of this.
backendLoop ::
  (MonadConc m) =>
  SharedRep m a ->
  (a -> Text) ->
  -- | initial code to place html of the SharedRep
  (Html () -> [Code]) ->
  -- | output step
  ((HashMap Text Text -> (HashMap Text Text, Either Text a)) -> (a -> Text) -> StateT (Int, HashMap Text Text) m [Code]) ->
  Box m [Code] (Text, Text) -> m ()
backendLoop sr out inputCode outputCode (Box c e) = flip evalStateT (0, HashMap.empty) $ do
  -- you only want to run unrep once for a SharedRep
  (Rep h fa) <- unrep sr
  b <- lift $ commit c (inputCode h)
  o <- outputCode fa out
  b' <- lift $ commit c o
  when (b && b') (go fa)
  where
    go fa = do
      incoming <- lift $ emit e
      modify (updateS incoming)
      o <- outputCode fa out
      b <- lift $ commit c o
      when b (go fa)
    updateS Nothing s = s
    updateS (Just (k,v)) s = second (insert k v) s

defaultInputCode :: Html () -> [Code]
defaultInputCode h = [Append "input" (toText h)]

defaultOutputCode :: (MonadIO m) => (HashMap Text Text -> (HashMap Text Text, Either Text a)) -> (a -> Text) -> StateT (Int, HashMap Text Text) m [Code]
defaultOutputCode fa out = do
  s <- get
  let (m', ea) = fa (snd s)
  modify (second (const m'))
  pure $
    [ case ea of
        Left err -> Append "debug" err
        Right a -> Replace "output" (out a)
    ]

wrangle :: Monad m => Box m Text Text -> Box m [Code] (Text,Text)
wrangle (Box c e) = Box c' e'
  where
    c' = listC $ contramap code c
    e' = mapE (pure . either (const Nothing) Just) (parseE parserJ e)

-- | {"event":{"element":"textid","value":"abcdees"}}
parserJ :: A.Parser (Text,Text)
parserJ = do
  _ <- A.string [q|{"event":{"element":"|]
  e <- A.takeTill (=='"')
  _ <- A.string [q|","value":"|]
  v <- A.takeTill (=='"')
  _ <- A.string [q|"}}|]
  pure (e,v)

-- * code hooks
-- * code messaging
data Code =
  Replace Text Text |
  Append Text Text |
  Console Text |
  Eval Text |
  Val Text
  deriving (Eq, Show, Generic, Read)

code :: Code -> Text
code (Replace i t) = replace i t
code (Append i t) = append i t
code (Console t) = console t
code (Eval t) = t
code (Val t) = val t

console :: Text -> Text
console t = [qc| console.log({t}) |]

val :: Text -> Text
val t = [qc| jsb.ws.send({t}) |]

-- | replace a container and run any embedded scripts
replace :: Text -> Text -> Text
replace d t =
      [qc|
     var $container = document.getElementById('{d}');
     $container.innerHTML = '{clean t}';
     runScripts($container);
     refreshJsb();
     |]

-- | append to a container and run any embedded scripts
append :: Text -> Text -> Text
append d t =
      [qc|
     var $container = document.getElementById('{d}');
     $container.innerHTML += '{clean t}';
     runScripts($container);
     refreshJsb();
     |]

clean :: Text -> Text
clean =
  Text.intercalate "\\'" . Text.split (== '\'')
    . Text.intercalate "\\n"
    . Text.lines

-- * initial javascript
-- | create a web socket for event data
webSocket :: PageJs
webSocket =
  PageJsText
    [q|
window.jsb = {ws: new WebSocket('ws://' + location.host + '/')};
jsb.event = function(ev) {
    jsb.ws.send(JSON.stringify({event: ev}));
};
jsb.ws.onmessage = function(evt){ 
    eval('(function(){' + evt.data + '})()');
};
|]

-- * scripts
-- | Event hooks that may need to be reattached given dynamic content creation.
refreshJsbJs :: PageJs
refreshJsbJs =
  PageJsText
    [q|
function refreshJsb () {
  $('.jsbClassEventInput').off('input');
  $('.jsbClassEventInput').on('input', (function(){
    jsb.event({ 'element': this.id, 'value': this.value});
  }));
  $('.jsbClassEventChange').off('change');
  $('.jsbClassEventChange').on('change', (function(){
    jsb.event({ 'element': this.id, 'value': this.value});
  }));
  $('.jsbClassEventFocusout').off('focusout');
  $('.jsbClassEventFocusout').on('focusout', (function(){
    jsb.event({ 'element': this.id, 'value': this.value});
  }));
  $('.jsbClassEventButton').off('click');
  $('.jsbClassEventButton').on('click', (function(){
    jsb.event({ 'element': this.id, 'value': this.value});
  }));
  $('.jsbClassEventToggle').off('click');
  $('.jsbClassEventToggle').on('click', (function(){
    jsb.event({ 'element': this.id, 'value': ('true' !== this.getAttribute('aria-pressed')).toString()});
  }));
  $('.jsbClassEventCheckbox').off('click');
  $('.jsbClassEventCheckbox').on('click', (function(){
    jsb.event({ 'element': this.id, 'value': this.checked.toString()});
  }));
  $('.jsbClassEventChooseFile').off('input');
  $('.jsbClassEventChooseFile').on('input', (function(){
    jsb.event({ 'element': this.id, 'value': this.files[0].name});
  }));
  $('.jsbClassEventShowSum').off('change');
  $('.jsbClassEventShowSum').on('change', (function(){
    var v = this.value;
    $(this).parent('.sumtype-group').siblings('.subtype').each(function(i) {
      if (this.dataset.sumtype === v) {
        this.style.display = 'block';
        } else {
        this.style.display = 'none';
      }
    })
  }));
  $('.jsbClassEventChangeMultiple').off('change');
  $('.jsbClassEventChangeMultiple').on('change', (function(){
    jsb.event({ 'element': this.id, 'value': [...this.options].filter(option => option.selected).map(option => option.value).join(',')});
  }));
};
|]

-- | prevent the Enter key from triggering an event
preventEnter :: PageJs
preventEnter =
  PageJs $
    parseJs
      [q|
window.addEventListener('keydown',function(e) {
  if(e.keyIdentifier=='U+000A' || e.keyIdentifier=='Enter' || e.keyCode==13) {
    if(e.target.nodeName=='INPUT' && e.target.type !== 'textarea') {
      e.preventDefault();
      return false;
    }
  }
}, true);
|]

-- | script injection js.
--
-- See https://ghinda.net/article/script-tags/ for why this might be needed.
runScriptJs :: PageJs
runScriptJs =
  PageJsText
    [q|
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
