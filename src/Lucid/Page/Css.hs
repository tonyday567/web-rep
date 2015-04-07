{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- could also be called Clay.Extended -}

module Lucid.Page.Css
  ( fill
  , stroke
  , strokeWidth
  , crispEdges
  , Lucid.Page.Css.optimizeSpeed
  , Lucid.Page.Css.geometricPrecision
  , shapeRendering
  , module Clay
  , pack
  ) where

import Clay hiding (optimizeSpeed, geometricPrecision, PlayState)
import Clay.Render
import Clay.Stylesheet (key)
import Control.Applicative
import Data.Monoid
import Data.Text (pack)
import Data.Text.Lazy (unpack)

instance Monoid Css where
  mempty = pure ()
  mappend = liftA2 mappend

instance Show Css where
  show = 
    unpack . 
    renderWith Config 
    { indentation    = "  "
    , newline        = "\n"
    , sep            = " "
    , finalSemicolon = True
    , warn           = True
    , align          = True
    , banner         = False
    } []

-- SVG css
fill :: Color -> Css
fill = key "fill"

stroke :: Color -> Css
stroke = key "stroke"

strokeWidth :: Size Abs -> Css
strokeWidth = key "stroke-width"

newtype ShapeRendering = ShapeRendering Value
  deriving (Inherit, Auto, Val)

crispEdges, optimizeSpeed, geometricPrecision :: ShapeRendering

crispEdges = ShapeRendering "crispEdges"
optimizeSpeed = ShapeRendering "optimizeSpeed"
geometricPrecision = ShapeRendering "geometricPrecision"

shapeRendering :: ShapeRendering -> Css
shapeRendering = key "shape-rendering"
