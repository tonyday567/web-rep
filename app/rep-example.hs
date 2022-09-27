{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE TupleSections #-}



import Web.Rep
import Web.Rep.Examples
import Prelude
import Options.Applicative
import Box
import Data.Text (pack)
import Data.Bifunctor
import qualified Lucid as L
import Optics.Core
import Control.Monad

data AppType =
  SharedTest |
  PlayTest |
  RestartTest
  deriving (Eq, Show)

data Options = Options
  { optionAppType :: AppType,
    optionBootstrapVersion :: BootstrapVersion
  }
  deriving (Eq, Show)

parseAppType :: Parser AppType
parseAppType =
  flag' SharedTest (long "shared" <> help "shared test") <|>
  flag' PlayTest (long "play" <> help "play functionality test") <|>
  flag' RestartTest (long "restart" <> help "console restart test") <|>
  pure SharedTest

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

-- | A simple count stream
countStream :: Int -> Double -> CoEmitter IO (Gap, [Code])
countStream n speed = fmap (second ((:[]) . Replace "output" . pack . show)) <$> qList (zip (0:repeat (1/speed)) [0..n])

main :: IO ()
main = do
  o <- execParser opts
  let a = optionAppType o
  let v = optionBootstrapVersion o
  case a of
    SharedTest -> defaultSharedServer v (maybeRep (Just "maybe") False repExamples)
    PlayTest -> playStreamWith (PlayConfig True 10 0) (defaultCodeBoxConfig & #codeBoxPage .~ replayPage) (countStream 100 1)
    RestartTest -> void restartTest

restartTest :: IO (Either Bool ())
restartTest = restart <$> (speedEffect (pure 1) . fmap (1,) <$> resetE 5 10) <*|> pure (glue showStdout . speedEffect (pure 1) <$|> countStream 100 1)

resetE :: Int -> Int -> CoEmitter IO Bool
resetE n m = qList (replicate (n-1) False <> [True] <> replicate m False)

runJustSpeed :: IO ()
runJustSpeed =
  playStreamSpeed (PlayConfig True 10 0) (countStream 100 1) <$|> codeBox

runJustPause :: IO ()
runJustPause =
  Box.close $ playStreamPause (PlayConfig True 10 0) <$> countStream 100 1 <*> codeBox

runNoReset :: IO ()
runNoReset = playStreamNoReset (PlayConfig True 10 0) (countStream 100 1) <$|> codeBox

-- | FIXME: document problem in the usage of <$|>
--
works :: IO (Either Bool ())
works = restart f io
  where
    io = glue showStdout . speedEffect (pure 1) <$|> countStream 20 1
    f = (== "q") <$> fromStdin

-- | Doesn't work here due to ythe floating of <$|> to the right. The IO process is 'continued' rather than restarted.
worksNot :: IO (Either Bool ())
worksNot = restart ((== "q") <$> fromStdin) . glue showStdout . speedEffect (pure 1) <$|> countStream 20 1

replayPage :: Page
replayPage = defaultSocketPage Boot5 & #htmlBody
      .~ divClass_
      "container"
      ( mconcat
          [ divClass_ "row" (L.h1_ "Replay Simulation"),
            divClass_ "row" . mconcat $ (\(t, h) -> divClass_ "col" (L.with L.div_ [L.id_ t] h)) <$>
                                         [ ("input", mempty),
                                           ("output", mempty)
                                         ]
          ]
      )
