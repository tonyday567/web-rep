{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}

-- import Control.Category (id)
import Lens.Micro
import Lucid
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Protolude hiding (replace)
import Web.Page
import Web.Page.Bootstrap
import Web.Page.Suavemente
import Web.Page.Examples
import Web.Page.Html.Input
import Web.Page.JSB
import Web.Scotty
import Data.Attoparsec.Text
import qualified Data.Text as Text
import Network.JavaScript
import qualified Box
import Data.Aeson (Value)
import qualified Data.Text.Lazy as Lazy
import qualified Control.Exception as E

inputTestPage :: Page
inputTestPage =
  bootstrapPage <>
  jsbPage &
  #htmlHeader .~ title_ "jsbTest" &
  #htmlBody .~ mconcat
  [ h1_ "inputTestPage"
  , with div_ [style_ "padding:1.5rem;position:relative;border-style:solid;border-color:#f8f9fa;border-width:0.2rem;"]
    (h2_ "inputs" <> with form_ [id_ "inputs"] mempty)
  , with div_ [id_ "output"] (h2_ "output" <> with div_ [id_ "results"] mempty)
  , with div_ [id_ "log"] (h2_ "server log")
  ]

rangeTest :: Input Int
rangeTest = jsbify $ bootify $
  Input
  3
  Range
  (Just "range example")
  Nothing
  "rangeid"
  [ ("style", "max-width:15rem;")
  , ("min", "0")
  , ("max", "5")
  , ("step", "1")
  ]

textTest :: Input Text
textTest = jsbify $ bootify $
  Input
  "abc"
  TextBox
  (Just "label")
  Nothing
  "textid"
  [ ("style", "max-width:15rem;")
  , ("placeholder", "test placeholder")
  ]

suaveTest :: Suave (Int, Text)
suaveTest = do
  s <- slider_ "slider" 0 5 3
  t <- textbox_ "label" "suave"
  pure (s,t)

initJsbTest :: (Int, Text)
initJsbTest = (rangeTest ^. #val, textTest ^. #val)

stepJsbTest :: Element -> (Int, Text) -> Either Text (Int, Text)
stepJsbTest (Element "rangeid" v) (_, t) =
  either
  (Left . Text.pack)
  (\x -> Right (x,t))
  p
  where
    p = parseOnly decimal v
stepJsbTest (Element "textid" v) (n, _) = Right (n,v)
stepJsbTest e _ = Left $ "unknown id: " <> show e

stepJsbTest' :: Element -> (Int, Text) -> (Int,Text)
stepJsbTest' e s =
  case stepJsbTest e s of
    Left _ -> s
    Right x -> x

sendResult :: Engine -> Either Text (Int, Text) -> IO ()
sendResult e (Left err) = append e "log" err
sendResult e (Right (i, v)) =
  replace e "results" (show i <> ":" <> v)

eventProcessTest :: Event Value -> Engine -> IO (Int, Text)
eventProcessTest ev e =
  eventConsume initJsbTest stepJsbTest'
  ( (Box.liftC <$> Box.showStdout) <>
    pure (Box.Committer (\v -> sendResult e v >> pure True))
  ) ev e

jsbMidTest :: (Show a) => Html () -> (Event Value -> Engine -> IO a) -> Application -> Application
jsbMidTest init eeio = start $ \ ev e -> do
  append e "inputs" (Lazy.toStrict $ renderText init)
  final <- eeio ev e `E.finally` putStrLn ("jsbMidTest finalled" :: Text)
  putStrLn $ ("final value was: " :: Text) <> show final

main :: IO ()
main =
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "other")
    -- middleware logStdoutDev
    -- middleware $ \app req res -> putStrLn ("raw path:" :: Text) >> print (rawPathInfo req) >> app req res
    -- middleware jsbMid
    -- middleware suaveMid
    -- middleware jsbBox
    -- middleware (suaveMid suaveTest (testSuave' suaveTest))
    middleware (jsbMidTest (toHtml rangeTest <> toHtml textTest) eventProcessTest)

    servePageWith "/" defaultPageConfig page1
    servePageWith "/default" defaultPageConfig page1
    servePageWith "/separated" cfg2 page2
    servePageWith "/jsb" defaultPageConfig inputTestPage
    -- get "/jsb" (html $ renderText $ renderPageHtmlWith defaultPageConfig jsbTest)



