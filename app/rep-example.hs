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

newtype Options = Options
  { optionAppType :: AppType
  }
  deriving (Eq, Show)

parseAppType :: Parser AppType
parseAppType =
  flag' SharedTest (long "shared" <> help "shared test")
    <|> pure SharedTest

options :: Parser Options
options =
  Options
    <$> parseAppType

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (fullDesc <> progDesc "web-rep testing" <> header "web-rep")

main :: IO ()
main = do
  o <- execParser opts
  let a = optionAppType o
  case a of
    SharedTest -> defaultSharedServer (maybeRep (Just "maybe") False repExamples)
