{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newTVarIO" #-}

-- | Data Type for incoming iqfeed admin
module Main where

import Box hiding (fileE)
import Control.Applicative hiding ((<|>))
import Control.Category
import Control.Monad.Conc.Class as C
import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.Text (Text, pack)
import Prelude hiding ((.))
import Optics.Core
import Web.Rep
import Lucid as L
import Control.Concurrent.Async
import Control.Monad.STM.Class
import Data.Bool
import Data.Hashable
import Data.HashMap.Strict as HashMap
import Data.Functor.Contravariant

replayPage :: Page
replayPage = defaultSocketPage Boot5 & #htmlBody
      .~ divClass_
      "container"
      ( mconcat
          [ divClass_ "row" (h1_ "Replay Simulation"),
            divClass_ "row" . mconcat $ ((\(t, h) -> divClass_ "col" (L.with div_ [id_ t] h)) <$>
                                         [ ("input", mempty),
                                           ("output", mempty)
                                         ])
          ]
      )

startSimX :: Int -> Double -> Committer IO [Code] -> IO ()
startSimX n speed c = glue c . fmap ((:[]) . Replace "output" . pack . show) . gapEffect <$|> qList (zip (0:repeat (1/speed)) [0..n])

simX :: Int -> Double -> CoEmitter IO [Code]
simX n speed = fmap ((:[]) . Replace "output" . pack . show) . gapEffect <$> qList (zip (0:repeat (1/speed)) [0..n])

startSimX' :: Int -> Double -> Committer IO [Code] -> IO ()
startSimX' n speed c = glue c <$|> simX n speed

backendLoopX ::
  SharedRep IO PlayConfig ->
  -- | [Code] is push instructions to change the page
  -- | (Text, Text) is the key-value pair for the shared representation
  Box IO [Code] (Text, Text) ->
  IO ()
backendLoopX sr (Box c e) = do
  refFaker <- C.newIORef Nothing -- Async ()
  flip evalStateT (0, HashMap.empty) $ do
    -- you only want to run unshare once for a SharedRep
    (Rep h fa) <- unshare sr
    b <- lift $ commit c [Replace "input" (toText h)]
    _ <- step' refFaker fa
    when b (go refFaker fa)
  where
    go ref fa = do
      incoming <- lift $ emit e
      modify (updateS incoming)
      _ <- step' ref fa
      go ref fa
    updateS Nothing s = s
    updateS (Just (k, v)) s = second (HashMap.insert k v) s
    step' ref fa = do
      s <- get
      let (m', ea) = fa (snd s)
      modify (second (const m'))
      liftIO $ case ea of
        (Right (PlayConfig togg speed _)) -> playX togg ref (startSimX 100 speed) c
        Left _ -> pure ()

iServer :: SharedRep IO PlayConfig -> SocketConfig -> Page -> IO ()
iServer srep scfg p =
  serveSocketBox scfg p
    <$|> fromAction (backendLoopX srep . wrangle)

main :: IO ()
main = iServer (repPlayConfig defaultPlayConfig) defaultSocketConfig replayPage

-- | play a committer action, based on a toggle
playX :: Bool -> C.IORef IO (Maybe (Async ())) -> (Committer IO [Code] -> IO ()) -> Committer IO [Code] -> IO ()
playX togg ref cio c = do
  s <- C.readIORef ref
  case (s, togg) of
    (Nothing, True) -> do
      a <- async $ cio c
      C.writeIORef ref (Just a)
    (Nothing, False) -> pure ()
    (Just _, True) -> pure ()
    (Just a, False) -> do
      cancel a
      C.writeIORef ref Nothing

-- | play a committer action, based on a toggle
playX' :: Bool -> C.IORef IO (Maybe (Async ())) -> (Box m b a -> IO ()) -> Box m b a -> IO ()
playX' togg ref k b = do
  s <- C.readIORef ref
  case (s, togg) of
    (Nothing, True) -> do
      a <- async (k b)
      C.writeIORef ref (Just a)
    (Nothing, False) -> pure ()
    (Just _, True) -> pure ()
    (Just a, False) -> do
      cancel a
      C.writeIORef ref Nothing


backendLoopX' ::
  SharedRep IO PlayConfig ->
  -- | [Code] is push instructions to change the page
  -- | (Text, Text) is the key-value pair for the shared representation
  Box IO [Code] (Text, Text) ->
  IO ()
backendLoopX' sr (Box c e) = do
  refFaker <- C.newIORef Nothing -- Async ()
  flip evalStateT (0, HashMap.empty) $ do
    -- you only want to run unshare once for a SharedRep
    (Rep h fa) <- unshare sr
    b <- lift $ commit c [Replace "input" (toText h)]
    _ <- step' refFaker fa
    when b (go refFaker fa)
  where
    go ref fa = do
      incoming <- lift $ emit e
      modify (updateS incoming)
      _ <- step' ref fa
      go ref fa
    updateS Nothing s = s
    updateS (Just (k, v)) s = second (HashMap.insert k v) s
    step' ref fa = do
      s <- get
      let (m', ea) = fa (snd s)
      modify (second (const m'))
      liftIO $ case ea of
        (Right (PlayConfig togg speed _)) -> playX togg ref (\c -> glue c <$|> simX 100 speed) c
        Left _ -> pure ()

step' :: (MonadState (a1, t) m, MonadIO m) => IO () -> (t -> (t, Either a2 PlayConfig)) -> m ()
step' x fa = do
      s <- get
      let (m', ea) = fa (snd s)
      modify (second (const m'))
      liftIO $ case ea of
        (Right PlayConfig {}) -> x
        Left _ -> pure ()

loop' :: (Hashable k, Monad m, MonadTrans t, MonadState (a1, HashMap k v) (t m), MonadIO (t m)) => Emitter m (k, v) -> IO () -> (HashMap k v -> (HashMap k v, Either a2 PlayConfig)) -> t m b
loop' e x fa = do
  incoming <- lift $ emit e
  modify (updateS incoming)
  _ <- step' x fa
  loop' e x fa
  where
    updateS Nothing s = s
    updateS (Just (k, v)) s = second (HashMap.insert k v) s


-- | A pauseable stream
-- > b = Box toStdout . fmap (pack . show) <$> (gapEffect <$> qList (zip (0:repeat (1::Double)) [1..100::Int]))
-- > pauseLoop True ((==" ") <$> fromStdin) <$|> b
--
pauseLoop :: Bool -> Emitter IO Bool -> Box IO a a -> IO ()
pauseLoop switch0 toggleE b = do
  switch <- atomically (newTVar switch0)
  _ <- race (togg toggleE switch) (pause switch b)
  pure ()

togg :: Emitter IO Bool -> TVar (STM IO) Bool -> IO ()
togg toggleE b = do
  incoming <- emit toggleE
  case incoming of
    Nothing -> pure ()
    Just incoming' -> atomically (writeTVar b incoming') >> togg toggleE b

togg' :: IO Bool -> TVar (STM IO) Bool -> IO ()
togg' b sw = do
  incoming' <- b
  atomically (writeTVar sw incoming')
  togg' b sw

pauseGlue :: Bool -> IO Bool -> Box IO a a -> IO ()
pauseGlue switch0 bm b = do
  switch <- atomically (newTVar switch0)
  _ <- race (togg' bm switch) (pause switch b)
  pure ()

pause :: TVar (STM IO) Bool -> Box IO a a -> IO ()
pause b (Box c e) = fix $ \rec -> do
  atomically $ do
    b' <- readTVar b
    check b'
  e' <- emit e
  c' <- maybe (pure False) (commit c) e'
  bool (pure ()) rec c'

evalSwitch :: Committer IO Bool -> Box IO [Code] (Text, Text) -> IO ()
evalSwitch cr (Box c e) = evalShared (repOnOff False) (contramap (\h -> [Replace "input" (toText h)]) c) (witherC (either (const (pure Nothing)) (pure . Just)) cr) e

evalShared :: Monad m => SharedRep m a -> Committer m (Html ()) -> Committer m (Either Text a) -> Emitter m (Text, Text) -> m ()
evalShared sr ch c e =
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

x1 :: Bool -> Emitter IO [Code] -> Box IO [Code] (Text, Text) -> IO ()
x1 switch0 pipe (Box c e) = do
  switch <- atomically (newTVar switch0)
  _ <- race
    (evalShared (repOnOff False) (contramap (\h -> [Replace "input" (toText h)]) c) (witherC (either (const (pure Nothing)) (pure . Just)) (Committer (\a -> atomically (writeTVar switch a) >> pure True) )) e)
    (pause switch (Box c pipe))
  pure ()

x2 :: IO ()
x2 = Box.close $ x1 False <$> simX 10000 100 <*> serveCodeBox defaultSocketConfig replayPage
