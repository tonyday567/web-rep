{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Page.Css
  ( fill
  , stroke
  , strokeWidth
  , crispEdges
  , optimizeSpeed
  , geometricPrecision
  , shapeRendering
  , Css
  , render
  , renderWith
  , compact
  , PageCss(..)
  ) where

import Clay hiding (optimizeSpeed, geometricPrecision, PlayState)
import Clay.Stylesheet (key)
import Data.Text
import Data.Text.Lazy (unpack)
import Protolude
import qualified GHC.Show

instance Show Css where
  show = Data.Text.Lazy.unpack . render

instance Eq Css where
  (==) a' b' = (Protolude.show a' :: Text) == Protolude.show b'

data PageCss = PageCss Css | PageCssText Text deriving (Eq, Show, Generic)

instance Semigroup PageCss where
  (<>) (PageCss css) (PageCss css') = PageCss (css <> css')
  (<>) (PageCssText css) (PageCssText css') = PageCssText (css <> css')
  (<>) (PageCss css) (PageCssText css') =
    PageCssText (show css <> css')
  (<>) (PageCssText css) (PageCss css') =
    PageCssText (css <> show css')

instance Monoid PageCss where
  mempty = PageCssText mempty
  mappend = (<>)

-- a few SVG css
fill :: Color -> Css
fill = key "fill"

stroke :: Color -> Css
stroke = key "stroke"

strokeWidth :: Size Position -> Css
strokeWidth = key "stroke-width"

newtype ShapeRendering = ShapeRendering Value
  deriving (Inherit, Auto, Val)

crispEdges, optimizeSpeed, geometricPrecision :: ShapeRendering
crispEdges = ShapeRendering "crispEdges"
optimizeSpeed = ShapeRendering "optimizeSpeed"
geometricPrecision = ShapeRendering "geometricPrecision"

shapeRendering :: ShapeRendering -> Css
shapeRendering = key "shape-rendering"
