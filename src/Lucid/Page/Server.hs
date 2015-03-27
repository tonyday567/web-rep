{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lucid.Page.Server
  ( module Lucid.Page.Server
  , module Happstack.Server
  ) where

import Control.Monad
import Data.ByteString.Char8 (pack)
import Happstack.Server
import Lucid (Html, renderBS)

-- | happstack server
serve :: ServerPart Response -> IO ()
serve response =
  simpleHTTP (nullConf {port = 8001}) $ do
    decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
    response

-- | include a directory to serve
includeDir :: String -> ServerPart Response -> ServerPart Response
includeDir lib page = msum
  [ dir "" page
  , dir lib $ serveDirectory EnableBrowsing [] lib
  ]

instance ToMessage (Html ()) where
  toContentType _ = pack "text/html; charset=UTF-8"
  toMessage       = renderBS

{-
servePageWith :: PageConfig -> Page -> ServerPart Response
servePageWith pc@(PageConfig concerns _ libs _) p = rs
  where
    rs = undefined
    case concerns of
      Inlined -> ok $ toResponse $ renderPageHtmlWith pc p
      Separated -> let (css,js,h) = renderPageWith pc p in
        msum
        [ ok $ toResponse h
        , dir "
  
-}
{-
servePage  :: ServerPart Response
servePage = msum
  [ dir "text" $ responsePlay textProducer textPage def
  , dir "line" $ responsePlay xyProducer linePage def
  , dir "circle" $ responsePlay abcProducer abcPage def
  , dir "scroll" $ responsePlay xyProducer scrollPage def
  , dir "scatter" $ responsePlay scatterProducer scatterPage def
  , dir "heatmap" $ responsePlay (heatmapProducer def) heatmapPage def
  , dir "bar" $ responsePlay barProducer barPage def
  , dir "barw" $ responsePlay barwProducer barWPage def
  , dir "barwadapt" $ responsePlay barWAdaptProducer barWAdaptPage def
  , dir "oned" $ responsePlay (quantileProducer 5) (oneDPage "quantiles" [] False) def
  , dir "oned-freq" $ responsePlay (freqProducer 5) (oneDPage "frequency version" [] True) def
  , dir "onedbar" oneDBarChart
  , dir "combo" $ responsePlay (comboProducer def) comboPage def
  , homePage
  ]
-}
