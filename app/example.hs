{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}

-- import Control.Category (id)
import Data.Aeson
import Lens.Micro
import Lucid
import Network.JavaScript
import Network.Wai
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Protolude hiding (replace)
import Web.Page
import Web.Page.Bootstrap
import Web.Page.Suavemente
import Web.Page.Examples
import Web.Page.Html.Input
import Web.Page.JSB
import Web.Scotty
import qualified Control.Exception as E
import qualified Data.Text.Lazy as Lazy

-- a page with bootstrap features to test the js bridge
jsbTest :: Page
jsbTest =
  bootstrapPage <>
  jsbPage &
  #htmlHeader .~ title_ "jsbTest" &
  #htmlBody .~ mconcat
  [ h1_ "jsbTest"
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

jsbListener :: Int -> Event Value -> Engine -> IO ()
jsbListener timeout ev e = do
  putStrLn ("jsbListener (re)started" :: Text)
  append e "log" "jsbListener (re)start ..."
  let x = renderText (toHtml rangeTest <> toHtml textTest)
  append e "inputs" (Lazy.toStrict x)
  _ <- addListener ev (result e)
  threadDelay (timeout * 1000 * 1000)
  replace e "log" "timed out"

suaveTester :: Suave Text
suaveTester = textbox_ "label" "suave"

suaveListener :: Int -> Event Value -> Engine -> IO ()
suaveListener timeout ev e = do
  putStrLn ("suaveListener (re)started" :: Text)
  append e "log" "suaveListener (re)start ..."
  Input' html' _ _ <- atomically $ evalStateT (suavely suaveTester) 0
  -- a0 <- atomically a
  append e "inputs" (Lazy.toStrict $ renderText html')
  _ <- addListener ev (result e)
  threadDelay (timeout * 1000 * 1000)
  replace e "log" "timed out"

suaveTest :: Page
suaveTest =
  bootstrapPage <>
  jsbPage &
  #htmlHeader .~ title_ "suaveTest" &
  #htmlBody .~ mconcat
  [ h1_ "suaveTest"
  , with div_ [style_ "padding:1.5rem;position:relative;border-style:solid;border-color:#f8f9fa;border-width:0.2rem;"]
    (h2_ "inputs" <> with form_ [id_ "inputs"] mempty)
  , with div_ [id_ "output"] (h2_ "output" <> with div_ [id_ "results"] mempty)
  , with div_ [id_ "log"] (h2_ "server log")
  ]

jsbMid :: Application -> Application
jsbMid = start $ \ ev e ->
  jsbListener 300 ev e `E.finally` putStrLn ("jsbListener finalled" :: Text)

suaveMid :: Application -> Application
suaveMid = start $ \ ev e ->
  suaveListener 300 ev e `E.finally` putStrLn ("suaveListener finalled" :: Text)

main :: IO ()
main =
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "other")
    middleware logStdoutDev
    middleware $ \app req res -> putStrLn ("raw path:" :: Text) >> print (rawPathInfo req) >> app req res
    -- middleware jsbMid
    middleware suaveMid

    servePageWith "/" defaultPageConfig page1
    servePageWith "/default" defaultPageConfig page1
    servePageWith "/separated" cfg2 page2
    servePageWith "/jsb" defaultPageConfig jsbTest
    servePageWith "/suave" defaultPageConfig suaveTest
    -- get "/jsb" (html $ renderText $ renderPageHtmlWith defaultPageConfig jsbTest)
