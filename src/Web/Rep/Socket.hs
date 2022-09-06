{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

-- | A socket between a web page and haskell, based on the box library.
module Web.Rep.Socket
  ( socketPage,
    serveSocketBox,
    sharedServer,
    defaultSharedServer,
    SocketConfig (..),
    defaultSocketConfig,
    defaultSocketPage,
    defaultInputCode,
    defaultOutputCode,
    Code (..),
    code,
    wrangle,
  backendLoop,
  backendLoop', defaultPlayConfig, repPlayConfig,
  PlayConfig (..),
  play,
  repPause,
  repReset,
  repSpeed,
  serveCodeBox,
  CodeBox,
  CoCodeBox,
  serveSocketCoBox)
where

import Box
import Control.Monad
import Control.Monad.Conc.Class as C
import Control.Monad.State.Lazy
import qualified Data.Attoparsec.Text as A
import Data.Bifunctor
import Data.Functor.Contravariant
import Data.HashMap.Strict as HashMap
import Data.Text (Text, pack)
import qualified Data.Text as Text
import GHC.Generics
import Lucid as L
import Network.Wai.Handler.WebSockets
import qualified Network.WebSockets as WS
import Optics.Core
import Text.InterpolatedString.Perl6
import Web.Rep.Bootstrap
import Web.Rep.Html
import Web.Rep.Page
import Web.Rep.Server
import Web.Rep.Shared
import Web.Scotty hiding (get)
import Box.Socket (serverApp)
import Data.Bool
import Control.Concurrent.Async
import Web.Rep.SharedReps

socketPage :: Page
socketPage =
  mempty & #jsOnLoad
    .~ mconcat
      [ webSocket,
        runScriptJs,
        refreshJsbJs,
        preventEnter
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

serveSocketBox :: SocketConfig -> Page -> Box IO Text Text -> IO ()
serveSocketBox cfg p b =
  scotty (cfg ^. #port) $ do
    middleware $ websocketsOr WS.defaultConnectionOptions (serverApp b)
    servePageWith "/" (defaultPageConfig "") p

type CodeBox = Box IO [Code] (Text, Text)

type CoCodeBox = Codensity IO (Box IO [Code] (Text, Text))

serveCodeBox :: SocketConfig -> Page -> CoCodeBox
serveCodeBox scfg p = wrangle <$> fromAction (serveSocketBox scfg p)

serveSocketCoBox :: SocketConfig -> Page -> CoBox IO Text Text -> IO ()
serveSocketCoBox cfg p b =
  scotty (cfg ^. #port) $ do
    middleware . websocketsOr WS.defaultConnectionOptions $ (\k -> Box.close $ serverApp <$> b <*> pure k)
    servePageWith "/" (defaultPageConfig "") p

sharedServer :: SharedRep IO a -> SocketConfig -> Page -> (Html () -> [Code]) -> (Either Text a -> IO [Code]) -> IO ()
sharedServer srep cfg p i o =
  serveSocketBox cfg p
    <$|> fromAction (backendLoop srep i o . wrangle)

defaultSharedServer :: (Show a) => BootstrapVersion -> SharedRep IO a -> IO ()
defaultSharedServer v srep =
  sharedServer srep defaultSocketConfig (defaultSocketPage v) defaultInputCode defaultOutputCode

defaultSocketPage :: BootstrapVersion -> Page
defaultSocketPage v =
  bool bootstrap5Page bootstrapPage (v==Boot4)
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

backendLoop ::
  (MonadConc m) =>
  SharedRep m a ->
  -- | initial code to place html of the SharedRep
  (Html () -> [Code]) ->
  -- | output code
  (Either Text a -> m [Code]) ->
  Box m [Code] (Text, Text) ->
  m ()
backendLoop sr inputCode outputCode (Box c e) = flip evalStateT (0, HashMap.empty) $ do
  -- you only want to run unshare once for a SharedRep
  (Rep h fa) <- unshare sr
  b <- lift $ commit c (inputCode h)
  o <- step' fa
  b' <- lift $ commit c o
  when (b && b') (go fa)
  where
    go fa = do
      incoming <- lift $ emit e
      modify (updateS incoming)
      o <- step' fa
      b <- lift $ commit c o
      when b (go fa)
    updateS Nothing s = s
    updateS (Just (k, v)) s = second (insert k v) s

    step' fa = do
      s <- get
      let (m', ea) = fa (snd s)
      modify (second (const m'))
      lift $ outputCode ea

data PlayConfig = PlayConfig
  { playPause :: Bool,
    playReset :: Bool,
    playSpeed :: Double,
    playFrame :: Int
  }
  deriving (Eq, Show, Generic)

defaultPlayConfig :: PlayConfig
defaultPlayConfig = PlayConfig True False 1 0

repPlayConfig :: PlayConfig -> SharedRep IO PlayConfig
repPlayConfig cfg =
  PlayConfig <$>
  repPause (view #playPause cfg) <*>
  repReset (view #playReset cfg) <*>
  repSpeed (view #playSpeed cfg) <*>
  repFrame (view #playFrame cfg)

repFrame :: Int -> SharedRep IO Int
repFrame x = read . Text.unpack <$> textbox (Just "frame") (pack $ show x)

repSpeed :: Double -> SharedRep IO Double
repSpeed x = sliderV (Just "speed") 0.1 100 0.01 x

repPause :: Bool -> SharedRep IO Bool
repPause initial = toggle_ (Just "play/pause") initial

repReset :: Bool -> SharedRep IO Bool
repReset initial = toggle_ (Just "stop/restart") initial

backendLoop' ::
  SharedRep IO PlayConfig ->
  -- | [Code] is push instructions to change the page
  -- | (Text, Text) is the key-value pair for the shared representation
  (Double -> Int -> Committer IO [Code] -> IO ()) ->
  CodeBox ->
  IO ()
backendLoop' sr ccode (Box c e) = do
  refFaker <- C.newIORef Nothing -- Async ()
  flip evalStateT (0, HashMap.empty) $ do
    -- you only want to run unshare once for a SharedRep
    (Rep h fa) <- unshare sr
    b <- lift $ commit c [Replace "input" (toText h)]
    step' refFaker fa
    when b (go refFaker fa)
  where
    go ref fa = do
      incoming <- lift $ emit e
      modify (updateS incoming)
      step' ref fa
      go ref fa
    updateS Nothing s = s
    updateS (Just (k, v)) s = second (HashMap.insert k v) s
    step' ref fa = do
      s <- get
      let (m', ea) = fa (snd s)
      modify (second (const m'))
      liftIO $ case ea of
        (Right (PlayConfig _ togg speed sk)) -> ccode speed sk (play togg ref c)
        Left _ -> pure ()

-- | a committer with a toggle
play :: Bool -> C.IORef IO (Maybe (Async Bool)) -> Committer IO a -> Committer IO a
play togg ref c = Committer $ \a -> do
  s <- C.readIORef ref
  case (s, togg) of
    (Nothing, True) -> do
      ref' <- async $ commit c a
      C.writeIORef ref (Just ref')
      pure True
    (Nothing, False) -> pure False
    (Just _, True) -> pure False
    (Just ref', False) -> do
      cancel ref'
      C.writeIORef ref Nothing
      pure False

defaultInputCode :: Html () -> [Code]
defaultInputCode h = [Append "input" (toText h)]

defaultOutputCode :: (Monad m, Show a) => Either Text a -> m [Code]
defaultOutputCode ea =
  pure $ case ea of
    Left err -> [Append "debug" err]
    Right a -> [Replace "output" (pack $ show a)]

wrangle :: Monad m => Box m Text Text -> Box m [Code] (Text, Text)
wrangle (Box c e) = Box c' e'
  where
    c' = listC $ contramap code c
    e' = witherE (pure . either (const Nothing) Just) (parseE parserJ e)

-- | attoparsec parse emitter which returns the original text on failure
parseE :: (Functor m) => A.Parser a -> Emitter m Text -> Emitter m (Either Text a)
parseE parser e = (\t -> either (const $ Left t) Right (A.parseOnly parser t)) <$> e

-- | {"event":{"element":"textid","value":"abcdees"}}
parserJ :: A.Parser (Text, Text)
parserJ = do
  _ <- A.string [q|{"event":{"element":"|]
  e <- A.takeTill (== '"')
  _ <- A.string [q|","value":"|]
  v <- A.takeTill (== '"')
  _ <- A.string [q|"}}|]
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
webSocket :: RepJs
webSocket =
  RepJsText
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
refreshJsbJs :: RepJs
refreshJsbJs =
  RepJsText
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
preventEnter :: RepJs
preventEnter =
  RepJs $
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
runScriptJs :: RepJs
runScriptJs =
  RepJsText
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
