{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newTVarIO" #-}

-- | Data Type for incoming iqfeed admin
module Main where

import Box hiding (fileE)
import Control.Applicative
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
import Data.HashMap.Strict as HashMap
import Data.Functor.Contravariant
import Data.Foldable

main :: IO ()
main =
  Box.close $
  playStream defaultPlayConfig <$>
  simX 10000 (view #playSpeed defaultPlayConfig) <*>
  serveCodeBox defaultSocketConfig replayPage

replayPage :: Page
replayPage = defaultSocketPage Boot5 & #htmlBody
      .~ divClass_
      "container"
      ( mconcat
          [ divClass_ "row" (h1_ "Replay Simulation"),
            divClass_ "row" . mconcat $ (\(t, h) -> divClass_ "col" (L.with div_ [id_ t] h)) <$>
                                         [ ("input", mempty),
                                           ("output", mempty)
                                         ]
          ]
      )

playStream :: PlayConfig -> Emitter IO (Gap, [Code]) -> Box IO [Code] (Text, Text) -> IO ()
playStream pcfg pipe (Box c e) = do
  ref <- newTVarConc pcfg
  _ <- race
    (sharedStream (repPlayConfig pcfg) (contramap (\h -> [Replace "input" (toText h)]) c) (witherC (either (const (pure Nothing)) (pure . Just)) (Committer (\a -> atomically (writeTVar ref a) >> pure True) )) e)
    (playS ref (Box c pipe))
  pure ()

simX :: Int -> Double -> CoEmitter IO (Gap, [Code])
simX n speed = fmap (second ((:[]) . Replace "output" . pack . show)) <$> qList (zip (0:repeat (1/speed)) [0..n])

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


-- > b = Box toStdout . fmap (pack . show) <$> (quitEffect (Emitter $ sleep 5 >> Just True) <$> gapEffect <$> qList (zip (0:repeat (1::Double)) [1..100::Int]))
quitEffect ::
  C.MonadConc m =>
  Emitter m Bool ->
  Emitter m a ->
  Emitter m a
quitEffect reset as =
  Emitter $ do
    r <- emit reset
    a <- emit as
    case (r,a) of
      (Just r', Just a') -> pure (bool (Just a') Nothing r')
      _ -> pure Nothing

resetEffect :: Monad m => Emitter m Bool -> Emitter m (Either Finish a) -> Emitter m a
resetEffect reset e =
  Emitter $ do
    r <- emit reset
    a <- emit e
    case (r,a) of
      (Just r', Just (Right a')) -> pure (bool Nothing (Just a') r')
      (_, Just (Left Finish)) -> pure Nothing
      _ -> pure Nothing

ef :: (MonadConc m, Alternative m) => Emitter m a -> Codensity m (Emitter m (Either Finish a))
ef e = (<>) (Right <$> e) <$> source 1 (pure (Left Finish))

ef' :: (MonadConc m, Alternative m) => Emitter m Bool -> Emitter m a -> CoEmitter m a
ef' reset e = resetEffect reset <$> ef e

efs :: (MonadConc m, Alternative m) => CoEmitter m a -> CoEmitter m (Either Finish a)
efs e = Data.Foldable.foldr (\a x -> (<|>) <$> x <*> a) mempty (replicate 4 (ef =<< e))

data Finish = Finish deriving (Show, Eq, Ord)

intE :: (MonadConc m) => Int -> CoEmitter m Int
intE n = qList [0..n]

killSwitch :: MonadConc m => Int -> CoEmitter m Bool
killSwitch n = qList (replicate n False <> [True])

qf :: (MonadConc m, Alternative m) => Int -> Int -> Codensity m (Emitter m Int)
qf k n = resetEffect <$> killSwitch k <*> (ef =<< intE n)

-- toListM <$|> ((<>) <$> (ef =<< (intE 4)) <*> (ef =<< intE 5))

repE :: (MonadConc m, Alternative m) => Emitter m a -> CoEmitter m (Either Finish a)
repE e = Data.Foldable.foldr (\x a -> (<>) <$> x <*> a) mempty (replicate 4 (ef e))

-- toListM <$|> Data.Foldable.foldr (\x a -> (<>) <$> x <*> a) mempty (replicate 4 (ef =<< (intE 3)))

-- FIXME: Emitter as an argument doesnt work
repE' :: (Monad m, Alternative m, MonadConc m) => Int -> CoEmitter m Int -> CoEmitter m (Either Finish Int)
repE' m e = Data.Foldable.foldr (\x a -> (<>) <$> x <*> a) mempty (replicate m (ef =<< e))

