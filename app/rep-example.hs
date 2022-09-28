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
{-# HLINT ignore "Use newtype instead of data" #-}



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
  RestartTest |
  ClosureBug
  deriving (Eq, Show)

data Options = Options
  { optionAppType :: AppType
  }
  deriving (Eq, Show)

parseAppType :: Parser AppType
parseAppType =
  flag' SharedTest (long "shared" <> help "shared test") <|>
  flag' PlayTest (long "play" <> help "play functionality test") <|>
  flag' RestartTest (long "restart" <> help "console restart test") <|>
  flag' ClosureBug (long "closure" <> help "documents the closure bug") <|>
  pure SharedTest

options :: Parser Options
options =
  Options
    <$> parseAppType

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
  case a of
    SharedTest -> sharedTest
    PlayTest -> playTest
    RestartTest -> void restartTest
    ClosureBug -> void closureBug

sharedTest :: IO ()
sharedTest =
  serveRep
  (maybeRep (Just "maybe") False repExamples)
  replaceInput
  replaceOutput
  defaultCodeBoxConfig

playTest :: IO ()
playTest = servePlayStream (PlayConfig True 10 0) (defaultCodeBoxConfig & #codeBoxPage .~ playPage) (countStream 100 1)

playPage :: Page
playPage = defaultSocketPage Boot5 & #htmlBody
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

restartTest :: IO (Either Bool ())
restartTest = restart <$> (gapEffect . fmap (1,) <$> resetE 5 10) <*|> pure (glue showStdout . gapEffect <$|> countStream 100 1)

resetE :: Int -> Int -> CoEmitter IO Bool
resetE n m = qList (replicate (n-1) False <> [True] <> replicate m False)

-- | documenting the issue with left floating compositions in the usage of <$|>
--
closureBug :: IO (Either Bool ())
closureBug = do
  putStrLn "restart ((== \"q\") <$> fromStdin) (glue showStdout . gapEffect <$|> countStream 10 2)"
  putStrLn "type 'q' to restart"
  restart ((== "q") <$> fromStdin) (glue showStdout . gapEffect <$|> countStream 10 2)
  putStrLn "buggy version"
  putStrLn "restart ((== \"q\") <$> fromStdin) . glue showStdout . gapEffect <$|> countStream 20 1"
  restart ((== "q") <$> fromStdin) . glue showStdout . gapEffect <$|> countStream 10 2
