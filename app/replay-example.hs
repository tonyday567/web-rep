{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Data Type for incoming iqfeed admin
module Main where

import Box hiding (fileE, gapEffect)
import Control.Applicative hiding ((<|>))
import Control.Category
import Control.Monad.Conc.Class as C
import Control.Monad.State.Lazy
import Data.Bifunctor
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, unpack, pack)
import GHC.Generics
import Prelude hiding ((.))
import Optics.Core
import Web.Rep hiding (PlayConfig (..))
import Lucid as L
import Control.Concurrent.Async

data AdminSimConfig = AdminSimConfig {
  simFile :: FilePath,
  simSpeed :: Double,
  initialState :: Bool,
  socket :: SocketConfig,
  page :: Page
  } deriving (Show, Generic)

defaultAdminSimConfig :: AdminSimConfig
defaultAdminSimConfig = AdminSimConfig "test/canned-admin.log" 10 False defaultSocketConfig socketPage
  where
    socketPage = defaultSocketPage Boot5 & #htmlBody
      .~ divClass_
      "container"
      ( mconcat
          [ divClass_ "row" (h1_ "Admin Simulation"),
            divClass_ "row" . mconcat $ ((\(t, h) -> divClass_ "col" (L.with div_ [id_ t] h)) <$> sections)
          ]
      )
    sections =
        [ ("input", mempty),
          ("output", mempty)
        ]

startSimX :: Int -> Double -> Committer IO [Code] -> IO ()
startSimX n speed c = glue c . fmap ((:[]) . Replace "output" . pack . show) . gapEffect <$|> qList (zip (0:repeat speed) [0..n])

backendLoopX ::
  FilePath ->
  SharedRep IO (Double, Bool) ->
  -- | [Code] is push instructions to change the page
  -- | (Text, Text) is the key-value pair for the shared representation
  Box IO [Code] (Text, Text) ->
  IO ()
backendLoopX _ sr (Box c e) = do
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
        (Right (speed, togg)) -> play togg ref (startSimX 100 speed) c
        Left _ -> pure ()

iServer :: SharedRep IO (Double, Bool) -> FilePath -> SocketConfig -> Page -> IO ()
iServer srep fp cfg p =
  serveSocketBox cfg p
    <$|> fromAction (backendLoopX fp srep . wrangle)

iSim :: AdminSimConfig -> IO ()
iSim cfg = iServer ((,) <$> repSpeed 1 <*> repOnOff True) (cfg ^. #simFile) (cfg ^. #socket) (cfg ^. #page)

iSim' :: IO ()
iSim' = iSim defaultAdminSimConfig

gapEffect ::
  C.MonadConc m =>
  Emitter m (Double, a) ->
  Emitter m a
gapEffect =
  witherE
    ( \(s, a) -> do
        sleep s
        pure (Just a)
    )

main :: IO ()
main = iSim defaultAdminSimConfig

-- | play a committer action, based on a toggle
play :: Bool -> C.IORef IO (Maybe (Async ())) -> (Committer IO [Code] -> IO ()) -> Committer IO [Code] -> IO ()
play togg ref cio c = do
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

data PlayConfig = PlayConfig
  { playToggle :: Bool,
    playSpeed :: Double,
    playFrame :: Int
  } deriving (Eq, Show, Generic)

defaultPlayConfig :: PlayConfig
defaultPlayConfig = PlayConfig False 1 0

repPlayConfig :: Bool -> Double -> Int -> SharedRep IO PlayConfig
repPlayConfig b s f = PlayConfig <$> repOnOff b <*> repSpeed s <*> repFrame f

repFrame :: Int -> SharedRep IO Int
repFrame x = read . unpack <$> textbox (Just "frame") (pack $ show x)

repSpeed :: Double -> SharedRep IO Double
repSpeed x = slider (Just "speed") 0.1 100 0.01 x

repOnOff :: Bool -> SharedRep IO Bool
repOnOff initial = toggle (Just "toggle") initial
