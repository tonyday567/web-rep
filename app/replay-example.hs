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
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, pack)
import Prelude hiding ((.))
import Optics.Core
import Web.Rep
import Lucid as L
import Control.Concurrent.Async
import Control.Monad.STM.Class
import Data.Bool

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


pauseLoop :: Bool -> Emitter IO Bool -> Box (STM IO) a a -> IO ()
pauseLoop b0 toggleE (Box c e) = do
  b <- atomically (newTVar b0)
  a0 <- async (togg toggleE b)
  a1 <- async (pause b c e)
  _ <- waitAnyCancel [a0,a1]
  pure ()

togg :: (MonadConc f) => Emitter f Bool -> TVar (STM f) Bool -> f ()
togg toggleE b = do
  incoming <- emit toggleE
  case incoming of
    Nothing -> pure ()
    Just incoming' -> atomically (writeTVar b incoming') >> togg toggleE b

pause :: MonadConc f => TVar (STM f) Bool -> Committer (STM f) a -> Emitter (STM f) a -> f ()
pause b c e = atomically $ fix $ \rec -> do
  b' <- readTVar b
  check b'
  e' <- emit e
  c' <- maybe (pure False) (commit c) e'
  bool (pure ()) rec c'

