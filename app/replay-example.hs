{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newTVarIO" #-}
{-# LANGUAGE TupleSections #-}

-- | Data Type for incoming iqfeed admin
module Main where

import Box hiding (fileE)
import Control.Applicative
import Control.Category
import Control.Monad.Conc.Class as C
import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.Text (pack)
import Prelude hiding ((.))
import Optics.Core
import Web.Rep
import Lucid as L

main :: IO ()
main =
  Box.close $
  playStreamSpeed defaultPlayConfig <$>
  simX 10000 (view #playSpeed defaultPlayConfig) <*>
  serveCodeBox defaultSocketConfig replayPage

run :: IO ()
run =
  Box.close $
  playStreamA defaultPlayConfig <$>
  simX 10000 (view #playSpeed defaultPlayConfig) <*>
  serveCodeBox defaultSocketConfig replayPage


runSpeed :: IO ()
runSpeed =
  Box.close $
  playStreamSpeed (PlayConfig True 2 10) <$>
  simX 10000 1 <*>
  serveCodeBox defaultSocketConfig replayPage

runB :: IO ()
runB =
  Box.close $
  playStreamB (PlayConfig True 2 10) <$>
  simX 10000 1 <*>
  serveCodeBox defaultSocketConfig replayPage

runC :: IO ()
runC =
  Box.close $
  playStreamC (PlayConfig True 2 10) <$>
  simX 10000 1 <*>
  serveCodeBox defaultSocketConfig replayPage

runD :: IO ()
runD =
  Box.close $
  playStreamD (PlayConfig True 2 10) <$>
  simX 10000 1 <*>
  serveCodeBox defaultSocketConfig replayPage

runE :: IO ()
runE =
  Box.close $
  playStreamE (PlayConfig True 2 10) <$>
  simX 10000 1 <*>
  serveCodeBox defaultSocketConfig replayPage

-- Behaviour of this is messy (ignores speed effects on restart?)
runECo :: IO ()
runECo =
  playStreamECo (PlayConfig True 2 10) (simX 10000 1)
  <$|> serveCodeBox defaultSocketConfig replayPage

runReset :: IO (Either () (Either Bool ()))
runReset =
  Box.close $
  playStreamReset <$>
  simX 10 1 <*>
  serveCodeBox defaultSocketConfig replayPage

runReset' :: IO (Either () (Either Bool ()))
runReset' =
  Box.close $
  playStreamReset' <$>
  simX' 100 1 <*>
  serveCodeBox defaultSocketConfig replayPage

runServe4 :: IO (Either () (Either Bool ()))
runServe4 =
  Box.close $
  playStreamReset' <$>
  simX' 100 1 <*>
  serve4

runResetOutside :: IO (Either () (Either Bool ()))
runResetOutside =
  Box.close $
  playStreamResetOutside
  io <$>
  serve4
  where
    io = glue showStdout . speedEffect (pure 1) <$|> simX' 100 1

runResetOutside' :: IO (Either () (Either Bool ()))
runResetOutside' =
  Box.close $
  playStreamResetOutside' <$>
  simX' 100 1 <*>
  serve4

runDecomp :: IO (Either () (Either Bool ()))
runDecomp =
  Box.close $
  playStreamDecomp' <$>
  simX' 100 1 <*>
  serve4


-- The problem is in playReseter and not in shared stream.
-- behaviour is:
-- reset committer looks ok, but this
--
-- - skips a single emit
-- - then, continues with original.
--
runPlayReseter :: IO (Either () (Either Bool ()))
runPlayReseter =
  Box.close $
  playReseter <$>
  simX' 100 1 <*>
  pure ((== "q") <$> fromStdin)

reseter' :: IO (Either () (Either Bool ()))
reseter' =
  Box.close $
  runReseter' <$>
  simX' 100 1 <*>
  pure showStdout <*>
  pure ((== "q") <$> fromStdin)

-- | problem is in restart'?
reseterNoExtra :: IO (Either Bool ())
reseterNoExtra =
  Box.close $
  runReseterNoExtra <$>
  simX' 20 1 <*>
  pure showStdout <*>
  pure ((== "q") <$> fromStdin)

-- | problem is in restart'?
runRestart :: IO (Either Bool ())
runRestart =
  Box.close $ playRestart ((== "q") <$> fromStdin) showStdout
  <$> (speedEffect'' . fmap (1,) <$> intE 20)

-- | problem is in restart'?
runRestartCo :: IO (Either Bool ())
runRestartCo =
  playRestartCo ((== "q") <$> fromStdin) showStdout (speedEffect'' . fmap (1,) <$> intE 20)

runRestart' :: IO (Either Bool ())
runRestart' = restart' (logE "reset emitter: " ((== "q") <$> fromStdin)) (glue (logC "pipe committer: " showStdout) . logE "pipe emitter: " . speedEffect'' . fmap (1,) <$|> intE 100)

-- | Problem is in the usage of <$|>
--
works :: IO (Either Bool ())
works = restart' flag io
  where
    io = glue showStdout . speedEffect (pure 1) <$|> simX' 20 1
    flag = (== "q") <$> fromStdin

-- | Doesn't work here due to ythe floating of <$|> to the right. The IO process is 'continued' rather than restarted.
worksNot :: IO (Either Bool ())
worksNot = restart' ((== "q") <$> fromStdin) . glue showStdout . speedEffect (pure 1) <$|> simX' 20 1

runDecompCo :: IO (Either () (Either Bool ()))
runDecompCo =
  playStreamDecompCo' (simX' 100 1) <$|> serve4

-- Behaviour of this is dodgyish, especially if reset button is pushed when not paused.
runECo' :: IO ()
runECo' =
  playStreamECo (PlayConfig True 10 0) (simX 100 1) <$|> serve4


-- | target main example
runReplayExample :: IO ()
runReplayExample = playStreamECo (PlayConfig True 10 0) (simX 100 1) <$|> codeBox

-- other stuff

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

simX :: Int -> Double -> CoEmitter IO (Gap, [Code])
simX n speed = fmap (second ((:[]) . Replace "output" . pack . show)) <$> qList (zip (0:repeat (1/speed)) [0..n])

simX' :: Int -> Double -> CoEmitter IO (Gap, [Code])
simX' n speed = fmap (bimap (/speed) ((:[]) . Replace "output" . pack . show)) <$> intGap n

intE :: (MonadConc m) => Int -> CoEmitter m Int
intE n = qList [0..n]

intGap :: (MonadConc m) => Int -> CoEmitter m (Gap, Int)
intGap n = fmap (1,) <$> qList [0..(n - 1)]

resetGap :: (MonadConc m) => Int -> CoEmitter m (Gap, Bool)
resetGap n = fmap (1,) <$> qList (replicate (n-1) False <> [True])

resetE :: (MonadConc m) => Int -> Int -> CoEmitter m Bool
resetE n m = qList (replicate (n-1) False <> [True] <> replicate m False)

killSwitch :: MonadConc m => Int -> CoEmitter m Bool
killSwitch n = qList (replicate n False <> [True])

-- io = glue showStdout . speedEffect (pure 2) <$|> (intGap 8)
-- rb = speedEffect (pure 2) <$> ((<>) <$> resetGap 5 <*> resetGap 20)
-- restart <$> rb <*|> pure io

-- Continues with the old emitter state
-- > restart' <$> (speedEffect'' . fmap (1,) <$> resetE 5 10) <*|> (glue showStdout . speedEffect'' . fmap (1,) <$> intE 100)

-- Properly restarts the emitter
-- > restart' <$> (speedEffect'' . fmap (1,) <$> resetE 5 10) <*|> pure (glue showStdout . speedEffect'' . fmap (1,) <$|> intE 100)
