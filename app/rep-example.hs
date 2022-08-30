{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

import Web.Rep
import Web.Rep.Examples
import Prelude
import Options.Applicative
import Box
import Data.Time
import Data.Text
import qualified Lucid as L
import Control.Monad.Conc.Class as C
import GHC.Generics
import Optics.Core
import Data.Fixed

data AppType = SharedTest deriving (Eq, Show)

data Options = Options
  { optionAppType :: AppType,
    optionBootstrapVersion :: BootstrapVersion
  }
  deriving (Eq, Show)

parseAppType :: Parser AppType
parseAppType =
  flag' SharedTest (long "shared" <> help "shared test")
    <|> pure SharedTest

parseBV :: Parser BootstrapVersion
parseBV =
  flag' Boot4 (long "boot4" <> help "version 4") <|>
  flag' Boot5 (long "boot5" <> help "version 5")
    <|> pure Boot5

options :: Parser Options
options =
  Options
    <$> parseAppType
    <*> parseBV

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (fullDesc <> progDesc "web-rep testing" <> header "web-rep")

main :: IO ()
main = do
  o <- execParser opts
  let a = optionAppType o
  let v = optionBootstrapVersion o
  case a of
    SharedTest -> defaultSharedServer v (maybeRep (Just "maybe") False repExamples)

countTest :: IO ()
countTest = sharedServer (repPlayConfig defaultPlayConfig) defaultSocketConfig defaultIPage defaultInputCode (iCodeE' 1000)

-- |
-- >>> toListM <$|> countingE 10
-- [(2022-06-06 00:00:00,0),(2022-06-06 00:00:01,1),(2022-06-06 00:00:02,2),(2022-06-06 00:00:03,3),(2022-06-06 00:00:04,4),(2022-06-06 00:00:05,5),(2022-06-06 00:00:06,6),(2022-06-06 00:00:07,7),(2022-06-06 00:00:08,8),(2022-06-06 00:00:09,9)]
countingE :: C.MonadConc m => Integer -> CoEmitter m (LocalTime, Integer)
countingE n = qList ((\x -> (addLocalTime (secondsToNominalDiffTime (MkFixed (1000000000000 * x))) t0, x)) <$> [0 .. (n - 1)])
  where
    t0 = LocalTime (fromGregorian 2022 6 6) (TimeOfDay 0 0 (MkFixed 0))
{-
iSim :: SimConfig -> IO ()
iSim cfg =
  backendLoop (repPlayConfig (view #playConfig cfg))
  (\speed skip c -> glue c <$|> (fmap iCode <$> (replay speed skip =<< countingE 100))) <$|>
  (wrangle <$>
   fromAction
   (serveSocketBox (view #socket cfg) (view #page cfg)))

-}

outputCodeI :: Either Text PlayConfig -> IO [Code]
outputCodeI ea =
  case ea of
    Left err -> pure [Append "debug" err]
    Right a -> pure [Replace "output" (pack $ show a)]

iCodeE :: Integer -> PlayConfig -> IO [Code]
iCodeE u cfg = fmap (maybe [] iCode) <$> emit <$|> (replay (view #playSpeed cfg) (view #playFrame cfg) =<< countingE u)

iCodeE' :: Integer -> Either Text PlayConfig -> IO [Code]
iCodeE' m cfg' = case cfg' of
  Left txt -> pure (iCode txt)
  Right cfg -> fmap (maybe [] iCode) <$> emit <$|> (replay (view #playSpeed cfg) (view #playFrame cfg) =<< countingE m)

iCode :: (Show a) => a -> [Code]
iCode x = ((: []) . Replace "output") . pack . show $ x

data SimConfig = SimConfig
  { playConfig :: PlayConfig,
    socket :: SocketConfig,
    page :: Page,
    maxFrames :: Integer
  }
  deriving (Show, Generic)

defaultSimConfigI :: SimConfig
defaultSimConfigI = SimConfig defaultPlayConfig defaultSocketConfig defaultIPage 1000

defaultIPage :: Page
defaultIPage =
  bootstrap5Page
    <> socketPage
    & #htmlBody
    .~ divClass_
      "container"
      ( mconcat
          [ divClass_ "row" . mconcat $ ((\(t, h, c) -> divClass_ "col" (L.with L.div_ [L.id_ t, L.class_ c] h)) <$> sections)
          ]
      )
  where
    sections =
      [ ("input", mempty, "w-50"),
        ("output", mempty, "")
      ]

sb :: Codensity IO (Box IO [Code] (Text, Text))
sb = wrangle <$> fromAction (serveSocketBox defaultSocketConfig defaultIPage)

sim :: (Either Text PlayConfig -> IO [Code]) -> IO ()
sim iE = backendLoop (repPlayConfig defaultPlayConfig) defaultInputCode iE <$|> sb

sim' :: IO ()
sim' = backendLoop' (repPlayConfig defaultPlayConfig) ie' <$|> sb

ie' :: Double -> Int -> Committer IO [Code] -> IO ()
ie' speed skip c = glue c <$|> (fmap iCode <$> (replay speed skip =<< countingE 4))

-- glue showStdout <$|> (replay 1 0 =<< countingE 100)
