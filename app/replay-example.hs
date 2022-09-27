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
main = runReplayExample

-- | target main example
runReplayExample :: IO ()
runReplayExample = playStream (PlayConfig True 10 0) (simX 100 1)

runJustSpeed :: IO ()
runJustSpeed =
  playStreamSpeed (PlayConfig True 10 0) (simX 100 1) <$|> codeBox

runJustPause :: IO ()
runJustPause =
  Box.close $ playStreamPause (PlayConfig True 10 0) <$> simX 100 1 <*> codeBox

runNoReset :: IO ()
runNoReset = playStreamNoReset (PlayConfig True 10 0) (simX 100 1) <$|> codeBox

-- | FIXME: document problem in the usage of <$|>
--
works :: IO (Either Bool ())
works = restart flag io
  where
    io = glue showStdout . speedEffect (pure 1) <$|> simX' 20 1
    flag = (== "q") <$> fromStdin

-- | Doesn't work here due to ythe floating of <$|> to the right. The IO process is 'continued' rather than restarted.
worksNot :: IO (Either Bool ())
worksNot = restart ((== "q") <$> fromStdin) . glue showStdout . speedEffect (pure 1) <$|> simX' 20 1

runReplayExampleNoReset :: IO ()
runReplayExampleNoReset = playStreamNoReset (PlayConfig True 10 0) (simX 100 1) <$|> codeBox


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
