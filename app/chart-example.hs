{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Chart.Core
import Chart.Hud
import Chart.Spot
import Chart.Svg
import Codec.Picture.Types
import Data.Aeson (Value)
import Data.Aeson.Types
import Data.Attoparsec.Text
import Data.Colour.SRGB
import Data.Generics.Product (field)
import Data.Text (pack, unpack)
import GHC.Base (String)
import Graphics.Svg.Types as Svg hiding (Point, Text)
import Lens.Micro
import Lucid
import Network.JavaScript
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import NumHask.Prelude
import Text.Blaze (preEscapedText)
import Web.Page
import Web.Page.Bootstrap
import Web.Page.Examples
import Web.Page.Html.Input
import qualified Web.Page.Html.Scooter as Scooter
import Web.Page.JSB
import Web.Scotty
import Web.Suavemente
import Web.Suavemente.Input
import qualified Box
import qualified Control.Exception as E
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
 
markupSvg :: ChartSvg Double -> Markup
markupSvg =
  preEscapedText .
  xmlToText .
  renderXml (Point (400.0 :: Double) 400)

pixelToColor :: (Ord a, Floating a) => PixelRGBA8 -> Colour a
pixelToColor (PixelRGBA8 r g b _) = sRGB24 r g b

colorToPixel :: (Ord a, Floating a, RealFrac a) => Colour a -> PixelRGBA8
colorToPixel c = let (RGB r g b) = toSRGB24 c in PixelRGBA8 r g b 255

inputTextStyle :: TextStyle -> Suave TextStyle
inputTextStyle s = do
  ts <- realSlider_ "size" 0.02 0.3 0.01 (s ^. field @"size")
  tc <- colorPicker_ "color" (pixelToColor $ s ^. field @"color")
  to' <- realSlider_ "opacity" 0 1 0.1 (s ^. field @"opacity")
  ta <- inputAnchor (s ^. field @"alignH")
  th <- realSlider_ "hsize" 0.2 1 0.05 (s ^. field @"hsize")
  tv <- realSlider_ "vsize" 0.5 2 0.05 (s ^. field @"vsize")
  tn <- realSlider_ "nudge1" (-0.5) 0.5 0.05 (s ^. field @"nudge1")
  trc <- maybeInput "rotation" (maybe False  (const True) (s ^. field @"rotation")) "trc"
    (realSlider_ "rotation" (-180) 180 10 (maybe 0 identity (s ^. field @"rotation")))
  pure $ TextStyle ts (colorToPixel tc) to' ta th tv tn trc

inputRectStyle :: RectStyle -> Suave RectStyle
inputRectStyle s = do
  bs <- realSlider_ "border size" 0.02 0.3 0.01 (s ^. field @"borderSize")
  bc <- colorPicker_ "border color" (pixelToColor $ s ^. field @"borderColor")
  bo <- realSlider_ "border opacity" 0 1 0.1 (s ^. field @"borderOpacity")
  c <- colorPicker_ "color" (pixelToColor $ s ^. field @"color")
  o <- realSlider_ "opacity" 0 1 0.1 (s ^. field @"opacity")
  pure $ RectStyle bs (colorToPixel bc) bo (colorToPixel c) o

inputText :: (Text, Point Double) -> Suave (Text, Point Double)
inputText (t, Point x y) = do
  t' <- textbox_ "text" (unpack t)
  x' <- realSlider "x" (-1) 1 0.01 x
  y' <- realSlider "y" (-1) 1 0.01 y
  pure (pack t', Point x' y')

gdata :: [[Point Double]]
gdata =
  [ dataXY sin (Range 0 (2*pi)) 30
  ]

gopts :: [GlyphStyle]
gopts =
  [ field @"borderSize" .~ 0.001 $
    field @"size" .~ 0.1 $
    defaultGlyphStyle
  ]

glyphsChart :: [Chart Double]
glyphsChart = zipWith (\d s -> Chart (GlyphA s) mempty (SpotPoint <$> d)) gdata gopts

data CanvasConfig = CanvasConfig
  { color :: PixelRGBA8
  , opacity :: Double
  } deriving (Eq, Show, Generic)

defaultCanvasConfig :: CanvasConfig
defaultCanvasConfig  = CanvasConfig grey 0.1

inputCanvasConfig :: CanvasConfig -> Suave CanvasConfig
inputCanvasConfig cfg = do
  canvasc <- colorPicker_ "Canvas Color" (pixelToColor (cfg ^. field @"color"))
  canvaso <- realSlider_ "Canvas Opacity" 0 0.2 0.01 (cfg ^. field @"opacity")
  pure $ CanvasConfig (colorToPixel canvasc) canvaso

inputTitle :: (FromJSON a, Real a, ToMarkup a, Show a, Fractional a) => Text -> Title a -> Suave (Title a)
inputTitle txt cfg = do
  ttext <- textbox_ "text" (unpack txt)
  tstyle <- inputTextStyle (cfg^. field @"style")
  tp <- inputPlace (show $ cfg ^. field @"place")
  ta <- inputAnchor TextAnchorMiddle -- (cfg ^. field @"align")
  b <- realSlider_ "buffer" 0 0.2 0.01 (cfg ^. field @"buff")
  pure $ Title (pack ttext) tstyle (toPlace tp) ta b

inputPlace :: String -> Suave String
inputPlace =
  dropdown_ "Placement" ((\x -> (x,x)) <$>
  [ "Top"
  , "Bottom"
  , "Left"
  , "Right"
  ])

toPlace :: (Eq a, IsString a) => a -> Place b
toPlace sh =
  case sh of
    "Top" -> PlaceTop
    "Bottom" -> PlaceBottom
    "Left" -> PlaceLeft
    "Right" -> PlaceRight
    _ -> PlaceTop

fromPlace :: (Eq a, IsString a) => Place b -> a
fromPlace p =
  case p of
    PlaceTop -> "Top"
    PlaceBottom -> "Bottom"
    PlaceLeft -> "Left"
    PlaceRight -> "Right"
    _ -> "wtf"

inputAnchor :: TextAnchor -> Suave TextAnchor
inputAnchor ta =
  stringToAnchor <$>
  dropdown_ "Anchor" ((\x -> (x,x)) <$>
  (anchorToString <$> [TextAnchorStart, TextAnchorMiddle, TextAnchorEnd]))
  (anchorToString ta)

data AxisConfig a = AxisConfig
  { abar :: Maybe (Bar a)
  , hasAuto :: Bool
  , tickN :: Int
  , tickSize :: Double
  , place :: Place Double
  } deriving (Eq, Show, Generic)

inputBar :: (FromJSON a, Real a, ToMarkup a, Show a, Fractional a) => Bar a -> Suave (Bar a)
inputBar cfg = do
  p <- inputPlace (fromPlace $ cfg ^. field @"place")
  r <- inputRectStyle (cfg ^. field @"rstyle")
  w <- realSlider_ "width" 0 0.1 0.01 (cfg ^. field @"wid")
  b <- realSlider_ "buffer" 0 0.2 0.01 (cfg ^. field @"buff")
  pure $ Bar (toPlace p) r w b

inputAxis :: (FromRatio a, FromJSON a, Real a, ToMarkup a, Show a, Fractional a) => AxisConfig a -> Suave (AxisConfig a)
inputAxis cfg = do
  b <-
    maybeInput
    "axis bar"
    (isJust (cfg ^. field @"abar"))
    "ab"
    (inputBar (maybe defaultBar identity (cfg ^. field @"abar")))
  hauto <- checkbox_ "auto" (cfg^. field @"hasAuto")
  tn <- slider_ "no. ticks" 0 20 (cfg ^. field @"tickN")
  ts <- realSlider_ "tick size" 0 0.2 0.01 (cfg ^. field @"tickSize")
  p <- inputPlace (show $ cfg ^. field @"place")
  pure $ AxisConfig b hauto tn ts (toPlace p)

defaultAxisConfig :: (FromRatio a, FromJSON a, Real a, ToMarkup a, Show a, Fractional a) => AxisConfig a
defaultAxisConfig = AxisConfig (Just defaultBar) True 8 0.1 PlaceBottom

data HudConfig a = HudConfig
  { canvas1 :: Maybe CanvasConfig
  , title1 :: Maybe (Title a)
  , axis1 :: Maybe (AxisConfig a)
  } deriving (Eq, Show, Generic)

defaultHudConfig :: (Chartable a, Real a, FromJSON a, ToMarkup a, Fractional a, Show a) => HudConfig a
defaultHudConfig =
  HudConfig
  (Just defaultCanvasConfig)
  (Just (defaultTitle "chart-svg automation testing"))
  (Just defaultAxisConfig)
  -- (field @"place" .~ PlaceLeft $ defaultAxisConfig)

hud :: HudConfig Double -> ViewBox Double -> [Chart Double] -> ChartSvg Double
hud cfg vb =
  hudSvg vb [canvas1, bar1 <> tick1, htitle]
  where
    canvas1 = maybe mempty (\cfg' -> canvas
      (blob
       (cfg' ^. field @"color")
       (cfg' ^. field @"opacity")) mempty)
      (cfg ^. field @"canvas1")
    bar1 = maybe mempty (\x -> maybe mempty (`bar` mempty) (x ^. field @"abar"))
      (cfg ^. field @"axis1")
    ts c = (field @"tstyle" .~ TickRound (TickFormatDefault, c ^. field @"tickN") :: Tick Double -> Tick Double) defaultTick
    autoTick c = Hud $ \vb' d a -> let (ts',das') = adjustTick defaultAutoOptions vb' a (ts c, mempty) in let (Hud hud) = tick ts' das' in hud vb' d a
    tick1 = maybe mempty autoTick (cfg ^. field @"axis1")
    htitle = maybe mempty (`title` mempty) (cfg ^. field @"title1")

inputGlyphShape :: Suave String
inputGlyphShape =
  dropdown "Glyph" ((\x -> (x,x)) <$>
  [ "Circle"
  , "Triangle"
  , "Square"
  , "Ellipse"
  , "Rectangle"
  , "Rounded Rectangle"
  , "Verticle Line"
  , "Horizontal Line"
  , "Smiley Face"
  ]) "Circle"

toGlyph :: (Eq a, IsString a) => a -> GlyphShape
toGlyph sh =
  case sh of
    "Circle" -> CircleGlyph
    "Square" -> SquareGlyph
    "Triangle" -> TriangleGlyph (Point (-1) 0) (Point 1 0) (Point 0 1)
    "Ellipse" -> EllipseGlyph 1.5
    "Rectangle" -> RectSharpGlyph 1.5
    "Rounded Rectangle" -> RectRoundedGlyph 1.5 0.1 0.1
    "Verticle Line" -> VLineGlyph 0.01
    "Horizontal Line" -> HLineGlyph 0.01
    "Smiley Face" -> SmileyGlyph
    _ -> CircleGlyph

glyphStyle :: (IsString s, Floating a, RealFrac a, Eq s) => s -> Double -> Double -> Colour a -> Colour a -> Double -> Double -> GlyphStyle
glyphStyle sh bsz sz gc gbc go gbo =
  field @"shape" .~ toGlyph sh $
  field @"borderSize" .~ bsz $
  field @"size" .~ sz $
  field @"color" .~ colorToPixel gc $
  field @"opacity" .~ go $
  field @"borderColor" .~ colorToPixel gbc $
  field @"borderOpacity" .~ gbo $
  defaultGlyphStyle

inputGlyphStyle :: Suave GlyphStyle
inputGlyphStyle = do
  sh <- inputGlyphShape
  sz <- realSlider "Glyph Size" 0 0.2 0.01 (0.04 :: Double)
  gc <- colorPicker "Glyph Color"
    (pixelToColor (defaultGlyphStyle ^. field @"color"))
  go <- realSlider "Glyph Opacity" 0 1 0.1 (1 :: Double)
  bsz <- realSlider "Glyph Border Size" 0 0.02 0.001 (0.004 :: Double)
  gbc <- colorPicker "Glyph Border Color"
    (pixelToColor (PixelRGBA8 64 64 64 255))
  gbo <- realSlider "Glyph Border Opacity" 0 1 0.1 (1 :: Double)
  pure (glyphStyle sh bsz sz gc gbc go gbo)

scootGlyphShape :: (Monad m) => Scooter.Scoot m GlyphShape
scootGlyphShape = toGlyph <$>
  Scooter.dropdown_ takeText "Glyph" ((\x -> (x,x)) <$>
  [ "Circle"
  , "Triangle"
  , "Square"
  , "Ellipse"
  , "Rectangle"
  , "Rounded Rectangle"
  , "Verticle Line"
  , "Horizontal Line"
  , "Smiley Face"
  ]) "Circle"

scootGlyphStyle :: (Monad m) => Scooter.Scoot m GlyphStyle
scootGlyphStyle = do
  sh <- scootGlyphShape
  sz <- Scooter.stepsliderR_ "Glyph Size" 0 0.2 0.01 (0.04 :: Double)
  gc <- colorToPixel <$> Scooter.color_ "Glyph Color"
    (pixelToColor (defaultGlyphStyle ^. field @"color"))
  go <- Scooter.stepsliderR_ "Glyph Opacity" 0 1 0.1 (1 :: Double)
  bsz <- Scooter.stepsliderR_ "Glyph Border Size" 0 0.02 0.001 (0.004 :: Double)
  gbc <- colorToPixel <$> Scooter.color_ "Glyph Border Color"
    (pixelToColor (PixelRGBA8 64 64 64 255))
  gbo <- Scooter.stepsliderR_ "Glyph Border Opacity" 0 1 0.1 (1 :: Double)
  pure (GlyphStyle sz gc go gbc gbo bsz sh)

main :: IO ()
main = do
  putStrLn ("Server 👍" :: Text)
  putStrLn ("point a client to localhost:8080/hud" :: Text)
  let cfg = defaultHudConfig :: HudConfig Double
  suavementely
    [ ("text", SomeSuave markupSvg $ do
        te1 <- inputText ("A text element", Point 0 0)
        te2 <- inputText ("Another text element", Point 1 1)
        tes <- inputTextStyle defaultTextStyle
        pure $ hud (cfg & field @"title1" .~
                    Just (defaultTitle "text example"))
          (aspect 1.5)
          [ Chart (TextA tes (fst <$> [te1, te2]))
            mempty
            (SpotPoint . snd <$> [te1, te2])])
    , ("hud", SomeSuave markupSvg $ do
          let gs = GlyphStyle 0.04 blue 1 grey 1 0.004 CircleGlyph
          t <- maybeInput "title" (maybe False (const True) (cfg ^. field @"title1")) "wrapt"
            (inputTitle "title testing" (maybe (defaultTitle "default") identity
                                         (cfg ^. field @"title1")))
          can <- maybeInput "canvas" (maybe False (const True) (cfg ^. field @"canvas1")) "wrapcon"
            (inputCanvasConfig (maybe defaultCanvasConfig identity (cfg ^. field @"canvas1")))
          a <- maybeInput "axis" (maybe False (const True) (cfg ^. field @"axis1")) "wrapaxis1"
            (inputAxis (maybe defaultAxisConfig identity (cfg ^. field @"axis1")))
          pure $ hud (cfg &
                      field @"canvas1" .~ can &
                      field @"axis1" .~ a &
                      field @"title1" .~ t)
            (aspect 1.5)
            [ Chart (GlyphA gs) mempty
              (SpotPoint <$> dataXY sin (Range 0 (2*pi)) 30)])
    , ("glyph", SomeSuave markupSvg $ do
          gs <- inputGlyphStyle
          pure $ hud (cfg & field @"title1" .~
                    Just (defaultTitle "glyph example"))
            (aspect 1.5)
            [ Chart (GlyphA gs) mempty
              (SpotPoint <$> dataXY sin (Range 0 (2*pi)) 30)])
    ]

