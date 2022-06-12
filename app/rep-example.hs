{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

import Web.Rep
import Web.Rep.Examples
import Prelude
import Options.Applicative

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
