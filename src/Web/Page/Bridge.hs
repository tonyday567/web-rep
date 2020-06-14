{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

-- | A streaming bridge between a web page and haskell.
module Web.Page.Bridge
  ( bridgePage,
    append,
    replace,
    bridge,
    sendConcerns,
    Engine,
    start,
    Application,
    valueConsume,
    sharedConsume,
    runList,
    runOnEvent,
    midShared,
    refreshJsbJs,
    runScriptJs,
  )
where

import Box
import Box.Cont ()
import qualified Control.Foldl as L
import Control.Lens
import Control.Monad.Morph
import Control.Monad.State
import Data.Aeson
import Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Data.Text (Text, pack)
import Data.Text.Lazy (fromStrict)
import GHC.Conc
import Lucid
import Network.JavaScript (Application, Engine, JavaScript (..), addListener, command, send, start)
import qualified Streaming.Prelude as S
import Text.InterpolatedString.Perl6
import Web.Page.Html
import Web.Page.Types
import Prelude hiding (init)

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

-- | create a web socket for event data
webSocket :: PageJs
webSocket =
  PageJsText
    [q|
window.jsb = {ws: new WebSocket('ws://' + location.host + '/')};
jsb.ws.onmessage = (evt) => eval(evt.data);
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

-- | componentry to kick off a javascript-bridge enabled page
bridgePage :: Page
bridgePage =
  mempty
    & #jsGlobal .~ (preventEnter <> refreshJsbJs)
    & #jsOnLoad .~ webSocket

sendc :: Engine -> Text -> IO ()
sendc e = send e . command . JavaScript . fromStrict

-- | replace a container and run any embedded scripts
replace :: Engine -> Text -> Text -> IO ()
replace e d t =
  send e $
    command
      [qc|
     var $container = document.getElementById('{d}');
     $container.innerHTML = '{clean t}';
     refreshJsb();
     |]

-- | append to a container and run any embedded scripts
append :: Engine -> Text -> Text -> IO ()
append e d t =
  send e $
    command
      [qc|
     var $container = document.getElementById('{d}');
     $container.innerHTML += '{clean t}';
     refreshJsb();
     |]

clean :: Text -> Text
clean =
  Text.intercalate "\\'" . Text.split (== '\'')
    . Text.intercalate "\\n"
    . Text.lines

-- | send css, js and html over the bridge
sendConcerns :: Engine -> Text -> Concerns Text -> IO ()
sendConcerns e t (Concerns c j h) = do
  replace e t h
  append e t (toText $ style_ c)
  sendc e j

-- | The javascript bridge continuation.
bridge :: Engine -> Cont_ IO Value
bridge e = Cont_ $ \vio -> void $ addListener e vio

fromJson' :: (FromJSON a) => Value -> Either Text a
fromJson' v = case fromJSON v of
  (Success a) -> Right a
  (Error e) -> Left $ "Json conversion error: " <> Text.pack e <> " of " <> (pack . show) v

valueModel :: (FromJSON a, MonadState s m) => (a -> s -> s) -> S.Stream (S.Of Value) m () -> S.Stream (S.Of (Either Text s)) m ()
valueModel step s =
  s
    & S.map fromJson'
    & S.partitionEithers
    & hoist (S.chain (modify . step))
    & hoist (S.mapM (const get))
    & S.unseparate
    & S.maps S.sumToEither

-- | consume an Element using a Committer and a Value continuation
valueConsume :: s -> (Element -> s -> s) -> Cont IO (Committer IO (Either Text s)) -> Cont_ IO Value -> IO s
valueConsume init step comm vio = do
  (c, e) <- atomically $ ends Unbounded
  with_ vio (atomically . c)
  etcM
    init
    (Transducer (valueModel step))
    (Box <$> comm <*> (liftE <$> pure (Emitter (Just <$> e))))

stepM :: MonadState s m => (s -> (s, b)) -> (a -> s -> s) -> a -> m (s, b)
stepM sr step v = do
  hm <- get
  let (hm', b) = sr $ step v hm
  put hm'
  pure (hm', b)

sharedModel :: (FromJSON a, MonadState s m) => (s -> (s, Either Text b)) -> (a -> s -> s) -> S.Stream (S.Of Value) m () -> S.Stream (S.Of (Either Text (s, Either Text b))) m ()
sharedModel sr step s =
  s
    & S.map fromJson'
    & S.partitionEithers
    & hoist (S.mapM (stepM sr step))
    & S.unseparate
    & S.maps S.sumToEither

-- | consume shared values using a step function, a continuation committer, and a Value continuation.
sharedConsume :: (s -> (s, Either Text b)) -> s -> (Element -> s -> s) -> Cont IO (Committer IO (Either Text (s, Either Text b))) -> Cont_ IO Value -> IO s
sharedConsume sh init step comm vio = do
  (c, e) <- atomically $ ends Unbounded
  with_ vio (atomically . c)
  etcM
    init
    (Transducer (sharedModel sh step))
    (Box <$> comm <*> (liftE <$> pure (Emitter (Just <$> e))))

-- | run a SharedRep using an initial state, a step function that consumes the shared model, and a value continuation
runOnEvent ::
  SharedRep IO a ->
  (Rep a -> StateT (Int, HashMap Text Text) IO ()) ->
  (Either Text (HashMap Text Text, Either Text a) -> IO ()) ->
  Cont_ IO Value ->
  IO (HashMap Text Text)
runOnEvent sr hio eaction cv = flip evalStateT (0, HashMap.empty) $ do
  (Rep h fa) <- unrep sr
  hio (Rep h fa)
  m <- zoom _2 get
  liftIO $
    sharedConsume
      fa
      m
      (\(Element k v) s -> insert k v s)
      (pure (Committer (\v -> eaction v >> pure True)))
      cv

-- | create Wai Middleware for a 'SharedRep' providing an initialiser and action on events
midShared ::
  () =>
  SharedRep IO a ->
  (Engine -> Rep a -> StateT (HashMap Text Text) IO ()) ->
  (Engine -> Either Text (HashMap Text Text, Either Text a) -> IO ()) ->
  Application ->
  Application
midShared sr init action = start $ \e ->
  void $
    runOnEvent
      sr
      (zoom _2 . init e)
      (action e)
      (bridge e)

-- | process a list of Values
runList ::
  (Monad m) =>
  SharedRep m a ->
  [Value] ->
  m [Either Text (HashMap Text Text, Either Text a)]
runList sr vs = S.fst' <$> do
  (faStep, (_, hm)) <- flip runStateT (0, HashMap.empty) $ do
    (Rep _ fa) <- unrep sr
    pure fa
  flip evalStateT hm $
    L.purely
      S.fold
      L.list
      (sharedModel faStep (\(Element k v) s -> insert k v s) (S.each vs))

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
