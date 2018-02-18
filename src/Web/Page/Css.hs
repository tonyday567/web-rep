{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Page.Css
  ( fill
  , stroke
  , strokeWidth
  , crispEdges
  , optimizeSpeed
  , geometricPrecision
  , shapeRendering
  , module Clay
  ) where

import Clay hiding (optimizeSpeed, geometricPrecision, PlayState)
import Clay.Stylesheet (key)
import Data.Text.Lazy (unpack)

instance Show Css where
  show = unpack . render

-- SVG css
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
