{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Web.Page.Server
  ( module Web.Page.Server
  , module Happstack.Server
  ) where

import Web.Page.Types
import Web.Page.Render
import Data.Monoid
import Happstack.Server
import Lucid (Html, renderBS)
import           Control.Monad
import           Data.Default
import Etc.Action
import Control.Monad.Managed

-- | happstack server
serve :: ServerPart Response -> IO ()
serve response =
  simpleHTTP (nullConf {port = 8001}) $ do
    decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
    response

-- | wrapped in an MVC.Action
serve' :: ServerPart Response -> IO ()
serve' = (\io -> with consoleActionBox $ \box -> controlBox def io box) . serve

-- | include a directory to serve
includeDir :: String -> ServerPart Response -> ServerPart Response
includeDir lib page = msum
  [ dir "" page
  , dir lib $ serveDirectory EnableBrowsing [] lib
  ]

instance ToMessage (Html ()) where
  toContentType _ = "text/html; charset=UTF-8"
  toMessage       = renderBS

servePage :: Page -> ServerPart Response
servePage p = ok $ toResponse $ renderPageHtmlWith def p

servePageWith :: PageConfig -> Page -> ServerPart Response
servePageWith pc@(PageConfig concerns _ _ locLibs (Concerns cssfp jsfp _)) p = pages <> servedir
  where
  pages = case concerns of
    Inline -> ok $ toResponse $ renderPageHtmlWith pc p
    Separated -> let (css,js,h) = renderPageWith pc p in
      msum
      [ dir cssfp $ ok $ toResponse css
      , dir jsfp $ ok $ toResponse js
      , dir "hello"    $ ok $ toResponse ("Hello, World!"::Html ())
      , dir "goodbye"  $ ok $ toResponse ("Goodbye, World!"::Html ())
      , ok $ toResponse h
      ]
  servedir = case locLibs of
    LinkedLibs -> mempty
    LocalLibs fp -> dir fp $ serveDirectory EnableBrowsing [] fp


