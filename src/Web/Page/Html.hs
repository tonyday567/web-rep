{-# LANGUAGE OverloadedStrings #-}

{- Could also be called Web.Extended -}

module Web.Page.Html 
  ( module Lucid
  , module Lucid.Base
  , doctypesvg_
  , svg11_
  , defs_
  , linksvg_
  ) where

import Lucid
import Lucid.Base

{- svg elements
-}
doctypesvg_  :: Monad m => HtmlT m ()
doctypesvg_ = makeElementNoEnd "?xml version='1.0' encoding='UTF-8'?><!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN\'\n'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'"

svg11_ :: Monad m => HtmlT m () -> HtmlT m ()
svg11_ = 
  with (makeElement "svg")
  [ makeAttribute "xmlns" "http://www.w3.org/2000/svg"
  , makeAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
  , makeAttribute "version" "1.1"
  ]

linksvg_ :: Monad m => HtmlT m () -> HtmlT m ()
linksvg_ = makeElement "link"

defs_ :: Monad m => HtmlT m a -> HtmlT m a
defs_ = makeElement "defs"

