{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

import Box
import Control.Monad
import Data.Bifunctor
import FlatParse.Basic (strToUtf8)
import Optics.Core
import Options.Applicative
import Web.Rep
import Web.Rep.Examples
import Prelude
import MarkupParse

data AppType
  = SharedTest
  | PlayTest
  | RestartTest
  | ClosureBug
  deriving (Eq, Show)

newtype Options = Options
  { optionAppType :: AppType
  }
  deriving (Eq, Show)

parseAppType :: Parser AppType
parseAppType =
  flag' SharedTest (long "shared" <> help "shared test")
    <|> flag' PlayTest (long "play" <> help "play functionality test")
    <|> flag' RestartTest (long "restart" <> help "console restart test")
    <|> flag' ClosureBug (long "closure" <> help "documents the closure bug")
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

-- | A simple count stream
countStream :: Int -> Double -> CoEmitter IO (Gap, [Code])
countStream n speed = fmap (second ((: []) . Replace "output" . strToUtf8 . show)) <$> qList (zip (0 : repeat (1 / speed)) [0 .. n])

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
playPage =
  defaultSocketPage
    & set #htmlBody
      (pure $ wrap "div" [Attr "class" "container"]
        (
            [ wrap "div" [Attr "class" "row"] [wrap "h1" [] [pure $ Content "replay simulation"]],
              wrap "div" [Attr "class" "row"] $
                (\(t, h) ->
                   wrap "div" [Attr "class" "row"]
                   (pure $ wrap "h2" [] (pure $ wrap "div" [Attr "id" t] [pure $ Content h]))) <$> sections
            ]
        ))
  where
    sections =
      [ ("input", mempty),
        ("output", mempty)
      ]

restartTest :: IO (Either Bool ())
restartTest = restart <$> (gapEffect . fmap (1,) <$> resetE 5 10) <*|> pure (glue showStdout . gapEffect <$|> countStream 100 1)

resetE :: Int -> Int -> CoEmitter IO Bool
resetE n m = qList (replicate (n - 1) False <> [True] <> replicate m False)

-- | documenting the issue with left floating compositions in the usage of <$|>
closureBug :: IO (Either Bool ())
closureBug = do
  putStrLn "restart ((== \"q\") <$> fromStdin) (glue showStdout . gapEffect <$|> countStream 10 2)"
  putStrLn "type 'q' to restart"
  restart ((== "q") <$> fromStdin) (glue showStdout . gapEffect <$|> countStream 10 2)
  putStrLn "buggy version"
  putStrLn "restart ((== \"q\") <$> fromStdin) . glue showStdout . gapEffect <$|> countStream 20 1"
  restart ((== "q") <$> fromStdin) . glue showStdout . gapEffect <$|> countStream 10 2
