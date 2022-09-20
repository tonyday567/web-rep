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

runSpeed :: IO ()
runSpeed =
  Box.close $
  playStreamSpeed (PlayConfig True 2 10) <$>
  simX 10000 1 <*>
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

simX :: Int -> Double -> CoEmitter IO (Gap, [Code])
simX n speed = fmap (second ((:[]) . Replace "output" . pack . show)) <$> qList (zip (0:repeat (1/speed)) [0..n])

intE :: (MonadConc m) => Int -> CoEmitter m Int
intE n = qList [0..n]

intGap :: (MonadConc m) => Int -> CoEmitter m (Gap, Int)
intGap n = fmap (1,) <$> qList [0..(n - 1)]

resetGap :: (MonadConc m) => Int -> CoEmitter m (Gap, Bool)
resetGap n = fmap (1,) <$> qList (replicate (n-1) False <> [True])

killSwitch :: MonadConc m => Int -> CoEmitter m Bool
killSwitch n = qList (replicate n False <> [True])

-- io = glue showStdout . speedEffect (pure 2) <$|> (intGap 8)
-- rb = speedEffect (pure 2) <$> ((<>) <$> resetGap 5 <*> resetGap 20)
-- restart <$> rb <*|> pure io
