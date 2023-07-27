{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | A socket between a web page and haskell, based on the box library.
module Web.Rep.Socket (socketPage, defaultSocketPage, SocketConfig (..), defaultSocketConfig, serveSocketBox, CodeBox, CoCodeBox, CodeBoxConfig (..), defaultCodeBoxConfig, codeBox, codeBoxWith, serveRep, serveRepWithBox, replaceInput, replaceOutput, replaceOutput_, sharedStream, PlayConfig (..), defaultPlayConfig, repPlayConfig, servePlayStream, servePlayStreamWithBox, parserJ, Code (..), code, console, val, replace, append, clean, webSocket, refreshJsbJs, preventEnter, runScriptJs) where

import Box
import Box.Socket (serverApp)
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.State.Lazy
import Data.Attoparsec.Text qualified as A
import Data.Bifunctor
import Data.Bool
import Data.Functor.Contravariant
import Data.HashMap.Strict as HashMap
import Data.Profunctor
import Data.Text (Text, pack)
import Data.Text qualified as Text
import GHC.Generics
import Lucid as L
import Network.Wai.Handler.WebSockets
import Network.WebSockets qualified as WS
import Optics.Core
import Data.String.Interpolate
import Web.Rep.Bootstrap
import Web.Rep.Html
import Web.Rep.Page
import Web.Rep.Server
import Web.Rep.Shared
import Web.Rep.SharedReps
import Web.Scotty (middleware, scotty)

-- | Page with all the trimmings for a sharedRep Box
socketPage :: Page
socketPage =
  mempty
    & #jsOnLoad
      .~ mconcat
        [ webSocket,
          runScriptJs,
          refreshJsbJs,
          preventEnter
        ]

defaultSocketPage :: BootstrapVersion -> Page
defaultSocketPage v =
  bool bootstrap5Page bootstrapPage (v == Boot4)
    <> socketPage
    & #htmlBody
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

-- | Socket configuration
--
-- >>> defaultSocketConfig
-- SocketConfig {host = "127.0.0.1", port = 9160, path = "/"}
data SocketConfig = SocketConfig
  { host :: Text,
    port :: Int,
    path :: Text
  }
  deriving (Show, Eq, Generic)

-- | official default
defaultSocketConfig :: SocketConfig
defaultSocketConfig = SocketConfig "127.0.0.1" 9160 "/"

-- | bidirectional websocket serving a 'Box'
serveSocketBox :: SocketConfig -> Page -> Box IO Text Text -> IO ()
serveSocketBox cfg p b =
  scotty (cfg ^. #port) $ do
    middleware $ websocketsOr WS.defaultConnectionOptions (serverApp b)
    servePageWith "/" (defaultPageConfig "") p

-- | A common Box pattern. [Code] is typically committed to the websocket and key-value elements, representing changes to the shared objects that are in the Dom are emitted.
type CodeBox = Box IO [Code] (Text, Text)

-- | Codensity CodeBox
type CoCodeBox = Codensity IO (Box IO [Code] (Text, Text))

-- | Configuration for a CodeBox serving.
data CodeBoxConfig = CodeBoxConfig
  { codeBoxSocket :: SocketConfig,
    codeBoxPage :: Page,
    codeBoxCommitterQueue :: Queue [Code],
    codeBoxEmitterQueue :: Queue (Text, Text)
  }
  deriving (Generic)

-- | official default config.
defaultCodeBoxConfig :: CodeBoxConfig
defaultCodeBoxConfig = CodeBoxConfig defaultSocketConfig (defaultSocketPage Boot5) Single Single

-- | Turn a configuration into a live (Codensity) CodeBox
codeBoxWith :: CodeBoxConfig -> CoCodeBox
codeBoxWith cfg =
  fromActionWith
    (view #codeBoxEmitterQueue cfg)
    (view #codeBoxCommitterQueue cfg)
    ( serveSocketBox (view #codeBoxSocket cfg) (view #codeBoxPage cfg)
        . dimap (either undefined id . A.parseOnly parserJ) (mconcat . fmap code)
    )

-- | Turn the default configuration into a live (Codensity) CodeBox
codeBox :: CoCodeBox
codeBox = codeBoxWith defaultCodeBoxConfig

-- | serve a SharedRep
serveRep :: SharedRep IO a -> (Html () -> [Code]) -> (Either Text a -> [Code]) -> CodeBoxConfig -> IO ()
serveRep srep i o cfg =
  serveRepWithBox srep i o <$|> codeBoxWith cfg

-- | non-codensity sharedRep server.
serveRepWithBox :: SharedRep IO a -> (Html () -> [Code]) -> (Either Text a -> [Code]) -> CodeBox -> IO ()
serveRepWithBox srep i o (Box c e) =
  sharedStream srep (contramap i c) (contramap o c) e

-- | Convert HTML representation to Code, replacing the input section of a page.
replaceInput :: Html () -> [Code]
replaceInput h = [Replace "input" (toText h)]

-- | Convert (typically parsed representation) to Code, replacing the output section of a page, and appending errors.
replaceOutput :: (Show a) => Either Text a -> [Code]
replaceOutput ea =
  case ea of
    Left err -> [Append "debug" err]
    Right a -> [Replace "output" (pack $ show a)]

-- | Convert (typically parsed representation) to Code, replacing the output section of a page, and throwing away errors.
replaceOutput_ :: (Show a) => Either Text a -> [Code]
replaceOutput_ ea =
  case ea of
    Left _ -> []
    Right a -> [Replace "output" (pack $ show a)]

-- | Stream a SharedRep
sharedStream ::
  (Monad m) => SharedRep m a -> Committer m (Html ()) -> Committer m (Either Text a) -> Emitter m (Text, Text) -> m ()
sharedStream sr ch c e =
  flip evalStateT (0, HashMap.empty) $ do
    -- you only want to run unshare once for a SharedRep
    (Rep h fa) <- unshare sr
    b <- lift $ commit ch h
    when b (go fa)
  where
    go fa = do
      e' <- lift $ emit e
      case e' of
        Nothing -> pure ()
        Just (k, v) -> do
          hmap <- snd <$> get
          let hmap' = insert k v hmap
          let (hmap'', r) = fa hmap'
          modify (second (const hmap''))
          b <- lift $ commit c r
          when b (go fa)

-- * Play

-- | Configuration to control a (re)play of an emitter with a Gap (timing) element.
data PlayConfig = PlayConfig
  { playPause :: Bool,
    playSpeed :: Double,
    playFrame :: Int
  }
  deriving (Eq, Show, Generic)

-- | Start on pause at normal speed and at frame 0.
defaultPlayConfig :: PlayConfig
defaultPlayConfig = PlayConfig True 1 0

-- | representation of a PlayConfig
repPlayConfig :: PlayConfig -> SharedRep IO PlayConfig
repPlayConfig cfg =
  PlayConfig
    <$> repPause (view #playPause cfg)
    <*> repSpeed (view #playSpeed cfg)
    <*> repFrame (view #playFrame cfg)

-- | representation of the playFrame in a PlayConfig
repFrame :: Int -> SharedRep IO Int
repFrame x = read . Text.unpack <$> textbox (Just "frame") (pack $ show x)

-- | representation of the playSpeed in a PlayConfig
repSpeed :: Double -> SharedRep IO Double
repSpeed x = sliderV (Just "speed") 0.5 100 0.5 x

-- | representation of the playPause toggle in a PlayConfig
repPause :: Bool -> SharedRep IO Bool
repPause initial = toggle_ (Just "play/pause") initial

-- | representation of a Bool reset button
repReset :: SharedRep IO Bool
repReset = button (Just "reset")

-- | Serve an emitter controlled by a PlayConfig representation, with an explicit CodeBox.
servePlayStreamWithBox :: PlayConfig -> CoEmitter IO (Gap, [Code]) -> CodeBox -> IO ()
servePlayStreamWithBox pcfg pipe (Box c e) = do
  (playBox, _) <- toBoxM (Latest (False, pcfg))
  race_
    (sharedStream ((,) <$> repReset <*> repPlayConfig pcfg) (contramap (\h -> [Replace "input" (toText h)]) c) (witherC (either (const (pure Nothing)) (pure . Just)) (committer playBox)) e)
    (restart (fst <$> emitter playBox) (glue c <$|> speedSkipEffect ((\x -> (playFrame (snd x), playSpeed (snd x))) <$> emitter playBox) . pauser (playPause . snd <$> emitter playBox) =<< pipe))
  pure ()

-- | Serve an emitter controlled by a PlayConfig representation.
servePlayStream :: PlayConfig -> CodeBoxConfig -> CoEmitter IO (Gap, [Code]) -> IO ()
servePlayStream pcfg cbcfg s = servePlayStreamWithBox pcfg s <$|> codeBoxWith cbcfg

-- * low-level JS conversions

-- | {"event":{"element":"textid","value":"abcdees"}}
parserJ :: A.Parser (Text, Text)
parserJ = do
  _ <- A.string [i|{"event":{"element":"|]
  e <- A.takeTill (== '"')
  _ <- A.string [i|","value":"|]
  v <- A.takeTill (== '"')
  _ <- A.string [i|"}}|]
  pure (e, v)

-- * code hooks

-- * code messaging

data Code
  = Replace Text Text
  | Append Text Text
  | Console Text
  | Eval Text
  | Val Text
  deriving (Eq, Show, Generic, Read)

code :: Code -> Text
code (Replace i t) = replace i t
code (Append i t) = append i t
code (Console t) = console t
code (Eval t) = t
code (Val t) = val t

console :: Text -> Text
console t = [i| console.log(#{t}) |]

val :: Text -> Text
val t = [i| jsb.ws.send(#{t}) |]

-- | replace a container and run any embedded scripts
replace :: Text -> Text -> Text
replace d t =
  [i|
     var $container = document.getElementById('#{d}');
     $container.innerHTML = '#{clean t}';
     runScripts($container);
     refreshJsb();
     |]

-- | append to a container and run any embedded scripts
append :: Text -> Text -> Text
append d t =
  [i|
     var $container = document.getElementById('#{d}');
     $container.innerHTML += '#{clean t}';
     runScripts($container);
     refreshJsb();
     |]

clean :: Text -> Text
clean =
  Text.intercalate "\\'"
    . Text.split (== '\'')
    . Text.intercalate "\\n"
    . Text.lines

-- * initial javascript

-- | create a web socket for event data
webSocket :: RepJs
webSocket =
  RepJsText
    [i|
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
refreshJsbJs :: RepJs
refreshJsbJs =
  RepJsText
    [i|
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
preventEnter :: RepJs
preventEnter =
  RepJs $
    parseJs
      [i|
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
runScriptJs :: RepJs
runScriptJs =
  RepJsText
    [i|
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
