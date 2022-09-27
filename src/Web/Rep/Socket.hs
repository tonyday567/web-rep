{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# HLINT ignore "Redundant <$>" #-}

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
  repSpeed,
  playStream,
  sharedStream,
  pause,
  playS,
  speedEffect,
  quitEffect,

  CodeBox,
  CoCodeBox,
  playStreamWith,
  quit,
  restart,
  pauser,
  checkE,
  playStreamSpeed,
  playStreamPause,
  changer,
  skipEffect,
  speedSkipEffect,
  repReset,
  parserJ,
  parseE,
  speedEffect',
  speedEffect'',
  codeBox,
  playStreamNoReset, CodeBoxConfig(..), defaultCodeBoxConfig)
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
import Web.Scotty ( middleware, scotty )
import Box.Socket (serverApp)
import Data.Bool
import Control.Concurrent.Async
import Web.Rep.SharedReps
import Control.Monad.STM.Class as C
import Data.Profunctor

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

data CodeBoxConfig = CodeBoxConfig
  { codeBoxSocket :: SocketConfig,
    codeBoxPage :: Page,
    codeBoxCommitterQueue :: Queue [Code],
    codeBoxEmitterQueue :: Queue (Text, Text)
  } deriving (Generic)

defaultCodeBoxConfig :: CodeBoxConfig
defaultCodeBoxConfig = CodeBoxConfig defaultSocketConfig (defaultSocketPage Boot5) Single Single

codeBoxWith :: CodeBoxConfig -> CoCodeBox
codeBoxWith cfg =
  fromActionWith (view #codeBoxEmitterQueue cfg) (view #codeBoxCommitterQueue cfg)
  (serveSocketBox (view #codeBoxSocket cfg) (view #codeBoxPage cfg) .
   dimap (either undefined id . A.parseOnly parserJ) (mconcat . fmap code))

codeBox :: CoCodeBox
codeBox = codeBoxWith defaultCodeBoxConfig

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
    playSpeed :: Double,
    playFrame :: Int
  }
  deriving (Eq, Show, Generic)

defaultPlayConfig :: PlayConfig
defaultPlayConfig = PlayConfig True 1 0

repPlayConfig :: PlayConfig -> SharedRep IO PlayConfig
repPlayConfig cfg =
  PlayConfig <$>
  repPause (view #playPause cfg) <*>
  repSpeed (view #playSpeed cfg) <*>
  repFrame (view #playFrame cfg)

repFrame :: Int -> SharedRep IO Int
repFrame x = read . Text.unpack <$> textbox (Just "frame") (pack $ show x)

repSpeed :: Double -> SharedRep IO Double
repSpeed x = sliderV (Just "speed") 0.5 100 0.5 x

repPause :: Bool -> SharedRep IO Bool
repPause initial = toggle_ (Just "play/pause") initial

repReset :: SharedRep IO Bool
repReset = button (Just "reset")

-- | replay example candidate
playStream' :: PlayConfig -> CoEmitter IO (Gap, [Code]) -> Box IO [Code] (Text, Text) -> IO ()
playStream' pcfg pipe (Box c e) = do
  (playBox, _) <- toBoxM (Latest (False, pcfg))
  race_
    (sharedStream ((,) <$> repReset <*> repPlayConfig pcfg) (contramap (\h -> [Replace "input" (toText h)]) c) (witherC (either (const (pure Nothing)) (pure . Just)) (committer playBox)) e)
    (restart (fst <$> emitter playBox) (glue c <$|> speedSkipEffect ((\x -> (playFrame (snd x), playSpeed (snd x))) <$> emitter playBox) =<< pauser (playPause . snd <$> emitter playBox) <$> pipe))
  pure ()

-- | Play an emitter stream
playStreamWith :: PlayConfig -> CodeBoxConfig -> CoEmitter IO (Gap, [Code]) -> IO ()
playStreamWith pcfg cbcfg s = playStream' pcfg s <$|> codeBoxWith cbcfg

playStream :: PlayConfig -> CoEmitter IO (Gap, [Code]) -> IO ()
playStream pcfg s = playStreamWith pcfg defaultCodeBoxConfig s

-- | just the play config
playStreamNoReset :: PlayConfig -> CoEmitter IO (Gap, [Code]) -> Box IO [Code] (Text, Text) -> IO ()
playStreamNoReset pcfg pipe (Box c e) = do
  (playBox, _) <- toBoxM (Latest pcfg)
  race_
    (sharedStream (repPlayConfig pcfg) (contramap (\h -> [Replace "input" (toText h)]) c) (witherC (either (const (pure Nothing)) (pure . Just)) (committer playBox)) e)
    (glue c <$|> speedSkipEffect ((\x -> (playFrame x, playSpeed x)) <$> emitter playBox) =<< pauser (playPause <$> emitter playBox) <$> pipe)
  pure ()

-- | just the speed effect
playStreamSpeed :: PlayConfig -> CoEmitter IO (Gap, [Code]) -> Box IO [Code] (Text, Text) -> IO ()
playStreamSpeed pcfg pipe (Box c e) = do
  (playBox, _) <- toBoxM (Latest pcfg)
  race_
    (sharedStream (repPlayConfig pcfg) (contramap (\h -> [Replace "input" (toText h)]) c) (witherC (either (const (pure Nothing)) (pure . Just)) (committer playBox)) e)
    (glue c <$|> speedEffect (playSpeed <$> emitter playBox) <$> pipe)
  pure ()

playStreamPause :: PlayConfig -> Emitter IO (Gap, [Code]) -> Box IO [Code] (Text, Text) -> IO ()
playStreamPause pcfg pipe (Box c e) = do
  (playBox, _) <- toBoxM (Latest pcfg)
  race_
    (sharedStream (repPlayConfig pcfg) (contramap (\h -> [Replace "input" (toText h)]) c) (witherC (either (const (pure Nothing)) (pure . Just)) (committer playBox)) e)
    (glue c (speedEffect (pure 1) $ pauser (playPause <$> emitter playBox) pipe))
  pure ()

pauser :: Emitter IO Bool -> Emitter IO a -> Emitter IO a
pauser b e = Emitter $ fix $ \rec -> do
  b' <- emit b
  case b' of
    Nothing -> pure Nothing
    Just False -> emit e
    Just True -> rec

changer :: (Eq a, MonadConc m) => a -> Emitter m a -> CoEmitter m Bool
changer a0 e = evalEmitter a0 $ Emitter $ do
  r <- lift $ emit e
  case r of
    Nothing -> pure Nothing
    Just r' -> do
      r'' <- get
      put r'
      pure (Just (r'==r''))

sharedStream ::
  Monad m => SharedRep m a -> Committer m (Html ()) -> Committer m (Either Text a) -> Emitter m (Text, Text) -> m ()
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
          Just (k,v) -> do
            hmap <- snd <$> get
            let hmap' = insert k v hmap
            let (hmap'', r) = fa hmap'
            modify (second (const hmap''))
            b <- lift $ commit c r
            when b (go fa)

pause :: TVar (STM IO) Bool -> Box IO a a -> IO ()
pause b (Box c e) = fix $ \rec -> do
  atomically $ do
    b' <- readTVar b
    check b'
  e' <- emit e
  c' <- maybe (pure False) (commit c) e'
  bool (pure ()) rec c'



playE :: TVar (STM IO) PlayConfig -> Emitter IO PlayConfig
playE ref = Emitter $ do
  p <- atomically $ do
    p' <- readTVar ref
    check (not $ view #playPause p')
    pure p'
  pure (Just p)

playS :: TVar (STM IO) PlayConfig -> Box IO a (Gap, a) -> IO ()
playS ref (Box c e) = fix $ \rec -> do
  e' <- emit (speedEffect (view #playSpeed <$> playE ref) e)
  c' <- maybe (pure False) (commit c) e'
  bool (pure ()) rec c'

-- | quit a process if an emitter signals
--
-- > quit <$> speedEffect (pure 2) <$> (resetGap 5) <*|> pure io
-- 0
-- 1
-- 2
-- 3
-- 4
-- Left True
quit :: Emitter IO Bool -> IO a -> IO (Either Bool a)
quit flag io = race (checkE flag) io

-- | restart a process if flagged
-- > io = glue showStdout . speedEffect (pure 2) <$|> (intGap 8)
-- > rb = speedEffect (pure 2) <$> ((<>) <$> resetGap 5 <*> resetGap 20)
-- > restart <$> rb <*|> pure io
--
restart :: Emitter IO Bool -> IO a -> IO (Either Bool a)
restart flag io = fix $ \rec -> do
  res <- quit flag io
  case res of
    Left True -> rec
    Left False -> pure (Left False)
    Right r -> pure (Right r)

checkE :: MonadConc m => Emitter m Bool -> m Bool
checkE e = fix $ \rec -> do
  a <- emit e
  -- atomically $ check (a == Just False)
  case a of
    Nothing -> pure False
    Just True -> pure True
    Just False -> rec

speedEffect ::
  C.MonadConc m =>
  Emitter m Gap ->
  Emitter m (Gap, a) ->
  Emitter m a
speedEffect speeds as =
  Emitter $ do
    s <- emit speeds
    a <- emit as
    case (s,a) of
      (Just s', Just (g, a')) -> sleep (g/s') >> pure (Just a')
      _ -> pure Nothing

speedEffect' ::
  C.MonadConc m =>
  Emitter m Gap ->
  Emitter m a ->
  Emitter m a
speedEffect' speeds as =
  Emitter $ do
    s <- emit speeds
    a <- emit as
    case (s,a) of
      (Just s', Just a') -> sleep s' >> pure (Just a')
      _ -> pure Nothing

speedEffect'' ::
  C.MonadConc m =>
  Emitter m (Gap, a) ->
  Emitter m a
speedEffect'' as =
  Emitter $ do
    a <- emit as
    case a of
      Just (s,a') -> sleep s >> pure (Just a')
      _ -> pure Nothing

-- | Only add a Gap if greater than the Int emitter
--
-- effect is similar to a fast-forward of the first n emits
skipEffect ::
  C.MonadConc m =>
  Emitter m Int ->
  Emitter m Gap ->
  CoEmitter m Gap
skipEffect n e = evalEmitter 0 $ Emitter $ do
    n' <- lift $ emit n
    e' <- lift $ emit e
    count <- get
    modify (1+)
    case (n', e') of
      (_, Nothing) -> pure Nothing
      (Nothing, _) -> pure Nothing
      (Just n'', Just e'') ->
        pure $ Just (bool e'' 0 (n'' >= count))

-- | Only add a Gap if greater than the Int emitter
--
-- effect is similar to a fast-forward of the first n emits
speedSkipEffect ::
  C.MonadConc m =>
  Emitter m (Int, Gap) ->
  Emitter m (Gap, a) ->
  CoEmitter m a
speedSkipEffect p e = evalEmitter 0 $ Emitter $ do
    p' <- lift $ emit p
    e' <- lift $ emit e
    count <- get
    modify (1+)
    case (p', e') of
      (_, Nothing) -> pure Nothing
      (Nothing, _) -> pure Nothing
      (Just (n,s), Just (g,a)) ->
        sleep (bool (g/s) 0 (n >= count)) >> pure (Just a)

-- > b = Box toStdout . fmap (pack . show) <$> (quitEffect (Emitter $ sleep 5 >> Just True) <$> gapEffect <$> qList (zip (0:repeat (1::Double)) [1..100::Int]))
quitEffect ::
  C.MonadConc m =>
  Emitter m Bool ->
  Emitter m a ->
  Emitter m a
quitEffect r as =
  Emitter $ do
    r' <- emit r
    a <- emit as
    case (r',a) of
      (Just r'', Just a') -> pure (bool (Just a') Nothing r'')
      _ -> pure Nothing

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
        (Right (PlayConfig togg speed sk)) -> ccode speed sk (play togg ref c)
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

-- * low-level conversions

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
