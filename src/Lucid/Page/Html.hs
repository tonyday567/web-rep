{-# LANGUAGE OverloadedStrings #-}

{- Could also be called Lucid.Extended -}

module Lucid.Page.Html 
  ( module Lucid.Page.Html
  , module Lucid
  , module Lucid.Base
  ) where

import Lucid
import Lucid.Base

{- svg elements
-}
doctypesvg_  :: Monad m => HtmlT m ()
doctypesvg_ = makeElementNoEnd "?xml version='1.0' encoding='UTF-8'?><!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN\'\n'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'"

xml_ :: Monad m => HtmlT m ()
xml_ = makeElementNoEnd "?xml version=\"1.0\" standalone=\"no\"?"

svgdef_ :: Monad m => HtmlT m () -> HtmlT m ()
svgdef_ = 
  with (makeElement "svg")
  [ makeAttribute "xmlns" "http://www.w3.org/2000/svg"
  , makeAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
  ]

linkSvg_ :: Monad m => HtmlT m () -> HtmlT m ()
linkSvg_ = makeElement "link"

defs_ :: Monad m => HtmlT m a -> HtmlT m a
defs_ = makeElement "defs"

