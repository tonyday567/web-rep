{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Category (id)
import Lucid
import Network.Wai
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Protolude hiding (replace, empty)
import Web.Page
import Web.Page.Bootstrap
import Web.Page.Examples
import Web.Page.Html.Input
import Web.Page.Html.Scooter
import Web.Page.JSB
import Web.Scotty
import Data.Attoparsec.Text
import qualified Data.Text as Text
import Network.JavaScript
import qualified Box
import Data.Aeson (Value)
import qualified Data.Text.Lazy as Lazy
import qualified Control.Exception as E
import Data.HashMap.Strict
import Data.Functor.Contravariant
import Chart.Core
import Chart.Spot
import Data.Generics.Product (field)
import Codec.Picture.Types
import Data.Colour.SRGB
import Chart.Hud
import Chart.Svg
import NumHask.Data.Range
import Graphics.Svg.Types (TextAnchor(..))
import Control.Category (id)
import Control.Lens
import Text.InterpolatedString.Perl6

pixelToColor :: (Ord a, Floating a) => PixelRGBA8 -> Colour a
pixelToColor (PixelRGBA8 r g b _) = sRGB24 r g b

colorToPixel :: (Ord a, Floating a, RealFrac a) => Colour a -> PixelRGBA8
colorToPixel c = let (RGB r g b) = toSRGB24 c in PixelRGBA8 r g b 255

inputTestPage :: Page
inputTestPage =
  showJs <>
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

inputTestPage' :: Page
inputTestPage' =
  showJs <>
  bootstrapPage <>
  jsbPage &
  #htmlHeader .~ title_ "jsbTest" &
  #htmlBody .~ container
  (mconcat
    [ row (h1_ "inputTestPage")
    , row (colSm (h2_ "inputs" <> with form_ [id_ "inputs"] mempty) <>
           colSm (with div_ [id_ "output"]
                  (h2_ "output" <>
                   with div_ [id_ "results"] mempty)))
    , row (with div_ [id_ "log"] (h2_ "server log"))
    ])

testPage :: Html () -> Page
testPage h =
  showJs <>
  bootstrapPage <>
  jsbPage &
  #htmlHeader .~ title_ "testPage" &
  #htmlBody .~ container
  (mconcat
    [ row (h1_ "testPage")
    , row (with div_ [id_ "test"] h)
    , row (with div_ [id_ "log"] (h2_ "server log"))
    ])

rangeTest :: Input Int
rangeTest = jsbify $ bootify $
  Input
  3
  Slider
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

sendResultShow :: (Show a) => Engine -> Either Text a -> IO ()
sendResultShow e (Left err) = append e "log" err
sendResultShow e (Right a) =
  replace e "results"
  (Lazy.toStrict $ renderText $ cardify [] mempty (Just "result")
    (toHtml  (show a :: Text)))

eventProcessTest :: Event Value -> Engine -> IO (Int, Text)
eventProcessTest ev e =
  eventConsume initJsbTest stepJsbTest'
  ( (Box.liftC <$> Box.showStdout) <>
    pure (Box.Committer (\v -> sendResultShow e v >> pure True))
  ) ev e

jsbMidTest :: (Show a) => Html () -> (Event Value -> Engine -> IO a) -> Application -> Application
jsbMidTest init eeio = start $ \ ev e -> do
  append e "inputs" (Lazy.toStrict $ renderText init)
  final <- eeio ev e `E.finally` putStrLn ("jsbMidTest finalled" :: Text)
  putStrLn $ ("final value was: " :: Text) <> show final

scooterTest :: (Monad m) => Scoot m (Int, Text)
scooterTest = do
  n <- slider_ "slider" 0 5 3
  t <- textbox_ "textbox" "sometext"
  pure (n, t)

scooterTestMaybe :: (Monad m) => Scoot m (Maybe (Int, Text))
scooterTestMaybe = maybeInput' "testMaybe" "wrapper" True scooterTest

scootMidTest :: (Show a) => Scoot IO a -> (a -> Text) -> Application -> Application
scootMidTest s rend = start $ \ ev e -> do
  (Scooter h fa, (_, hm)) <- flip runStateT (0,empty) (unscoot s)
  append e "inputs" (Lazy.toStrict $ renderText h)
  final <- scootConsume (logResults rend) hm (faBridge fa) ev e
    `E.finally` putStrLn ("scootMidTest finalled" :: Text)
  putStrLn $ ("final value was: " :: Text) <> show final

faBridge :: (HashMap Text Text -> Either Text a) -> Either Text (HashMap Text Text) -> Either Text a
faBridge fa hm = either Left fa hm

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

scootGlyphShape :: (Monad m) => Scoot m GlyphShape
scootGlyphShape = toGlyph <$>
  dropdown'_ takeText show "Shape"
  [ "Circle"
  , "Triangle"
  , "Square"
  , "Ellipse"
  , "Rectangle"
  , "Rounded Rectangle"
  , "Verticle Line"
  , "Horizontal Line"
  , "Smiley Face"
  ] "Circle"

scootGlyphStyle :: (Monad m) => Scoot m GlyphStyle
scootGlyphStyle = cardifyScoot [("style", "width: 10 rem;")] mempty (Just "Glyph Style") $ do
  sh <- scootGlyphShape
  sz <- stepsliderR_ "Size" 0 0.2 0.01 (0.04 :: Double)
  gc <- colorToPixel <$> color_ "Color"
    (pixelToColor (defaultGlyphStyle ^. field @"color"))
  go <- stepsliderR_ "Opacity" 0 1 0.1 (1 :: Double)
  bsz <- stepsliderR_ "Border Size" 0 0.02 0.001 (0.004 :: Double)
  gbc <- colorToPixel <$> color_ "Border Color"
    (pixelToColor (PixelRGBA8 64 64 64 255))
  gbo <- stepsliderR_ "Border Opacity" 0 1 0.1 (1 :: Double)
  pure (GlyphStyle sz gc go gbc gbo bsz sh)

scootCheckbox :: (Monad m) => Scoot m Bool
scootCheckbox = checkbox_ "checkbox test" True

chartMidTest :: (Show a) => Scoot IO a -> (a -> Text) -> Application -> Application
chartMidTest sc rend = start $ \ ev e -> do
  ((Scooter h fa), (_, hm)) <- flip runStateT (0,empty) (unscoot sc)
  append e "inputs" (Lazy.toStrict $ renderText h)
  either (const $ append e "log" "fa hm stuffed up") (results rend e) (fa hm)
  final <- scootConsume (logResults rend) hm (faBridge fa) ev e
    `E.finally` putStrLn ("finalled" :: Text)
  putStrLn $ ("final value was: " :: Text) <> show final

renderChart gs cfg =
  xmlToText . renderXml (Point (600.0 :: Double) 400) $ chartTest gs cfg

renderChartGs gs = renderChart gs (defaultHudConfig & field @"title1" .~ Just (defaultTitle "glyph test"))

results :: (a -> Text) -> Engine -> a -> IO ()
results r e x = replace e "results" (r x)

logResults :: (a -> Text) -> Engine -> Either Text a -> IO ()
logResults _ e (Left err) = append e "log" err
logResults r e (Right x) = results r e x

scootConsume cc hm fa ev e =
  eventConsume hm (\(Element k v) s -> insert k v s)
  (contramap fa <$>
  ( (Box.liftC <$> Box.showStdout) <>
    pure (Box.Committer (\v -> cc e v >> pure True))
  )) ev e

chartTest :: GlyphStyle -> HudConfig Double -> ChartSvg Double
chartTest gs cfg =
  hud cfg
  (aspect 1.5)
  [ Chart (GlyphA gs) mempty
    (SpotPoint <$> dataXY sin (Range 0 (2*pi)) 30)
  ]

hudTest :: GlyphStyle -> HudConfig Double -> ChartSvg Double
hudTest gs hc =
  hud hc
  (aspect 1.5)
  [ Chart (GlyphA gs) mempty
    (SpotPoint <$> dataXY sin (Range 0 (2*pi)) 30)]


inputTitle :: (Monad m) => Text -> Title Double -> Scoot m (Title Double)
inputTitle txt cfg = do
  ttext <- textbox_ "text" txt
  tstyle <- inputTextStyle (cfg^. field @"style")
  tp <- inputPlace (cfg ^. field @"place")
  ta <- inputAnchor TextAnchorMiddle -- (cfg ^. field @"align")
  b <- stepsliderR_ "buffer" 0 0.2 0.01 (cfg ^. field @"buff")
  pure $ Title ttext tstyle tp ta b

inputTextStyle :: (Monad m) => TextStyle -> Scoot m TextStyle
inputTextStyle s = do
  ts <- stepsliderR_ "size" 0.02 0.3 0.01 (s ^. field @"size")
  tc <- color_ "color" (pixelToColor $ s ^. field @"color")
  to' <- stepsliderR_ "opacity" 0 1 0.1 (s ^. field @"opacity")
  ta <- inputAnchor (s ^. field @"alignH")
  th <- stepsliderR_ "hsize" 0.2 1 0.05 (s ^. field @"hsize")
  tv <- stepsliderR_ "vsize" 0.5 2 0.05 (s ^. field @"vsize")
  tn <- stepsliderR_ "nudge1" (-0.5) 0.5 0.05 (s ^. field @"nudge1")
  trc <- maybeInput "rotation" "trc" (maybe False  (const True) (s ^. field @"rotation"))
    (stepsliderR_ "rotation" (-180) 180 10 (maybe 0 identity (s ^. field @"rotation")))
  pure $ TextStyle ts (colorToPixel tc) to' ta th tv tn trc

inputPlace :: (Show a, Monad m) => Place a -> Scoot m (Place a)
inputPlace =
  dropdown'_ (toPlace <$> takeText) fromPlace "Placement"
  [ "Top"
  , "Bottom"
  , "Left"
  , "Right"
  ]

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

instance ToHtml TextAnchor where
  toHtml = toHtml . (anchorToString :: TextAnchor -> Text)
  toHtmlRaw = toHtmlRaw . (anchorToString :: TextAnchor -> Text)

inputAnchor :: (Monad m) => TextAnchor -> Scoot m TextAnchor
inputAnchor =
    dropdown'_
    (stringToAnchor <$> takeText)
    anchorToString
    "Anchor"
    (anchorToString <$> [TextAnchorStart, TextAnchorMiddle, TextAnchorEnd])

data CanvasConfig = CanvasConfig
  { color :: PixelRGBA8
  , opacity :: Double
  } deriving (Eq, Show, Generic)

defaultCanvasConfig :: CanvasConfig
defaultCanvasConfig  = CanvasConfig grey 0.1

data AxisConfig a = AxisConfig
  { abar :: Maybe (Bar a)
  , hasAuto :: Bool
  , tickN :: Int
  , tickSize :: Double
  , place :: Place Double
  } deriving (Eq, Show, Generic)

defaultAxisConfig :: AxisConfig Double
defaultAxisConfig = AxisConfig (Just defaultBar) True 8 0.1 PlaceBottom

data HudConfig a = HudConfig
  { canvas1 :: Maybe CanvasConfig
  , title1 :: Maybe (Title Double)
  , axis1 :: Maybe (AxisConfig Double)
  } deriving (Eq, Show, Generic)

defaultHudConfig :: HudConfig Double
defaultHudConfig =
  HudConfig
  (Just defaultCanvasConfig)
  (Just (defaultTitle "chart-svg automation testing"))
  (Just defaultAxisConfig)

inputCanvasConfig :: (Monad m) => CanvasConfig -> Scoot m CanvasConfig
inputCanvasConfig cfg = do
  canvasc <- color_ "Canvas Color" (pixelToColor (cfg ^. field @"color"))
  canvaso <- stepsliderR_ "Canvas Opacity" 0 0.2 0.01 (cfg ^. field @"opacity")
  pure $ CanvasConfig (colorToPixel canvasc) canvaso

inputRectStyle :: (Monad m) => RectStyle -> Scoot m RectStyle
inputRectStyle s = do
  bs <- stepsliderR_ "border size" 0.02 0.3 0.01 (s ^. field @"borderSize")
  bc <- color_ "border color" (pixelToColor $ s ^. field @"borderColor")
  bo <- stepsliderR_ "border opacity" 0 1 0.1 (s ^. field @"borderOpacity")
  c <- color_ "color" (pixelToColor $ s ^. field @"color")
  o <- stepsliderR_ "opacity" 0 1 0.1 (s ^. field @"opacity")
  pure $ RectStyle bs (colorToPixel bc) bo (colorToPixel c) o

inputBar :: (Monad m) => Bar Double -> Scoot m (Bar Double)
inputBar cfg = do
  p <- inputPlace (cfg ^. field @"place")
  r <- inputRectStyle (cfg ^. field @"rstyle")
  w <- stepsliderR_ "width" 0 0.1 0.01 (cfg ^. field @"wid")
  b <- stepsliderR_ "buffer" 0 0.2 0.01 (cfg ^. field @"buff")
  pure $ Bar p r w b

inputAxisConfig :: (Monad m) => AxisConfig Double -> Scoot m (AxisConfig Double)
inputAxisConfig cfg = do
  b <-
    maybeInput'
    "axis bar"
    "ab"
    (isJust (cfg ^. field @"abar"))
    (inputBar (maybe defaultBar identity (cfg ^. field @"abar")))
  hauto <- checkbox_ "auto" (cfg^. field @"hasAuto")
  tn <- slider_ "no. ticks" 0 20 (cfg ^. field @"tickN")
  ts <- stepsliderR_ "tick size" 0 0.2 0.01 (cfg ^. field @"tickSize")
  p <- inputPlace (cfg ^. field @"place")
  pure $ AxisConfig b hauto tn ts p

inputHudConfig :: (Monad m) => HudConfig Double -> Scoot m (HudConfig Double)
inputHudConfig cfg = do
  t <- maybeInput' "title" "xt" (isJust (cfg ^. #title1)) $
    inputTitle "label for title" (maybe (defaultTitle "actual title") id (cfg ^. #title1))
  can <- maybeInput' "canvas" "xc" (isJust (cfg ^. #canvas1)) $
    inputCanvasConfig (maybe defaultCanvasConfig id (cfg ^. #canvas1))
  ax <- maybeInput' "axis" "xa" (isJust (cfg ^. #axis1)) $
    inputAxisConfig (maybe defaultAxisConfig id (cfg ^. #axis1))
  pure (HudConfig can t ax)

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

scootChart :: (Monad m) => HudConfig Double -> Scoot m (GlyphStyle, HudConfig Double)
scootChart cfg = Scoot $ do
  t@(Scooter rt _) <- unscoot $ inputTitle "label for title" (maybe (defaultTitle "actual title") id (cfg ^. #title1))
  can@(Scooter rcan _) <- unscoot $ inputCanvasConfig (maybe defaultCanvasConfig id (cfg ^. #canvas1))
  ax@(Scooter rax _) <- unscoot $ inputAxisConfig (maybe defaultAxisConfig id (cfg ^. #axis1))
  gs@(Scooter rgs _) <- unscoot $ scootGlyphStyle
  acc <- zoom _1 $
    accordion "accScootChart" (Just "canvas") [("glyph style", rgs), ("canvas", rcan), ("title", rt), ("axis", rax)]
  pure (first
        (const acc) $
        (\a b c d -> (a, HudConfig (Just b) (Just c) (Just d))) <$> gs <*> can <*> t <*> ax)

main :: IO ()
main = do
  ah <- flip evalStateT 0 (accordion "acctest" Nothing $ (\x -> (show x, "filler")) <$> [1..3])
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "other")
    -- middleware logStdoutDev
    -- middleware $ \app req res -> putStrLn ("raw path:" :: Text) >> print (rawPathInfo req) >> app req res
    -- middleware jsbMid
    -- middleware suaveMid
    -- middleware jsbBox
    -- middleware (suaveMid suaveTest (testSuave' suaveTest))
    -- middleware (jsbMidTest (toHtml rangeTest <> toHtml textTest) eventProcessTest)
    -- middleware (scootMidTest scooterTestMaybe)
    middleware 
      ( chartMidTest (scootChart defaultHudConfig)
        (\(gs,cfg) -> renderChart gs cfg))
    servePageWith "/" defaultPageConfig page1
    servePageWith "/default" defaultPageConfig page1
    servePageWith "/separated" cfg2 page2
    servePageWith "/jsb" defaultPageConfig inputTestPage'

    servePageWith "/accordion" defaultPageConfig (testPage ah) 
    servePageWith "/acc1" defaultPageConfig (testPage (toHtmlRaw acc1)) 
    servePageWith "/acc2" defaultPageConfig (testPage (toHtmlRaw acc2)) 
    servePageWith "/acc3" defaultPageConfig (testPage (toHtmlRaw acc3)) 
    -- get "/jsb" (html $ renderText $ renderPageHtmlWith defaultPageConfig jsbTest)

acc1 :: Text
acc1 = [q|
<div class="accordion" id="accordionExample">
  <div class="card">
    <div class="card-header" id="headingOne">
      <h2 class="mb-0">
        <button class="btn btn-link" type="button" data-toggle="collapse" data-target="#collapseOne" aria-expanded="true" aria-controls="collapseOne">
          Collapsible Group Item #1
        </button>
      </h2>
    </div>

    <div id="collapseOne" class="collapse show" aria-labelledby="headingOne" data-parent="#accordionExample">
      <div class="card-body">
        Anim pariatur cliche reprehenderit, enim eiusmod high life accusamus terry richardson ad squid. 3 wolf moon officia aute, non cupidatat skateboard dolor brunch. Food truck quinoa nesciunt laborum eiusmod. Brunch 3 wolf moon tempor, sunt aliqua put a bird on it squid single-origin coffee nulla assumenda shoreditch et. Nihil anim keffiyeh helvetica, craft beer labore wes anderson cred nesciunt sapiente ea proident. Ad vegan excepteur butcher vice lomo. Leggings occaecat craft beer farm-to-table, raw denim aesthetic synth nesciunt you probably haven't heard of them accusamus labore sustainable VHS.
      </div>
    </div>
  </div>
  <div class="card">
    <div class="card-header" id="headingTwo">
      <h2 class="mb-0">
        <button class="btn btn-link collapsed" type="button" data-toggle="collapse" data-target="#collapseTwo" aria-expanded="false" aria-controls="collapseTwo">
          Collapsible Group Item #2
        </button>
      </h2>
    </div>
    <div id="collapseTwo" class="collapse" aria-labelledby="headingTwo" data-parent="#accordionExample">
      <div class="card-body">
        Anim pariatur cliche reprehenderit, enim eiusmod high life accusamus terry richardson ad squid. 3 wolf moon officia aute, non cupidatat skateboard dolor brunch. Food truck quinoa nesciunt laborum eiusmod. Brunch 3 wolf moon tempor, sunt aliqua put a bird on it squid single-origin coffee nulla assumenda shoreditch et. Nihil anim keffiyeh helvetica, craft beer labore wes anderson cred nesciunt sapiente ea proident. Ad vegan excepteur butcher vice lomo. Leggings occaecat craft beer farm-to-table, raw denim aesthetic synth nesciunt you probably haven't heard of them accusamus labore sustainable VHS.
      </div>
    </div>
  </div>
  <div class="card">
    <div class="card-header" id="headingThree">
      <h2 class="mb-0">
        <button class="btn btn-link collapsed" type="button" data-toggle="collapse" data-target="#collapseThree" aria-expanded="false" aria-controls="collapseThree">
          Collapsible Group Item #3
        </button>
      </h2>
    </div>
    <div id="collapseThree" class="collapse" aria-labelledby="headingThree" data-parent="#accordionExample">
      <div class="card-body">
        Anim pariatur cliche reprehenderit, enim eiusmod high life accusamus terry richardson ad squid. 3 wolf moon officia aute, non cupidatat skateboard dolor brunch. Food truck quinoa nesciunt laborum eiusmod. Brunch 3 wolf moon tempor, sunt aliqua put a bird on it squid single-origin coffee nulla assumenda shoreditch et. Nihil anim keffiyeh helvetica, craft beer labore wes anderson cred nesciunt sapiente ea proident. Ad vegan excepteur butcher vice lomo. Leggings occaecat craft beer farm-to-table, raw denim aesthetic synth nesciunt you probably haven't heard of them accusamus labore sustainable VHS.
      </div>
    </div>
  </div>
</div>
|]

acc2 :: Text
acc2 = [q|
<div class="accordion" id="a1">
  <div class="card">
    <div class="card-header" id="headingOne">
      <h2 class="mb-0">
        <button class="btn btn-link" type="button" data-toggle="collapse" data-target="#collapseOne" aria-expanded="true" aria-controls="collapseOne">
          1
        </button>
      </h2>
    </div>

    <div id="collapseOne" class="collapse show" aria-labelledby="headingOne" data-parent="#a1">
      <div class="card-body">
        filler
      </div>
    </div>
  </div>
  <div class="card">
    <div class="card-header" id="headingTwo">
      <h2 class="mb-0">
        <button class="btn btn-link collapsed" type="button" data-toggle="collapse" data-target="#collapseTwo" aria-expanded="false" aria-controls="collapseTwo">
          2
        </button>
      </h2>
    </div>
    <div id="collapseTwo" class="collapse" aria-labelledby="headingTwo" data-parent="#a1">
      <div class="card-body">
        filler
      </div>
    </div>
  </div>
  <div class="card">
    <div class="card-header" id="headingThree">
      <h2 class="mb-0">
        <button class="btn btn-link collapsed" type="button" data-toggle="collapse" data-target="#collapseThree" aria-expanded="false" aria-controls="collapseThree">
          3
        </button>
      </h2>
    </div>
    <div id="collapseThree" class="collapse" aria-labelledby="headingThree" data-parent="#a1">
      <div class="card-body">
        filler
      </div>
    </div>
  </div>
</div>
|]

acc3 :: Text
acc3 = [q|
<div id="acctest1" class="accordion"><div class="card"><div id="acctest2" class="card-header"><h2 class="mb-0"><button data-toggle="collapse" data-target="#acctest3" aria-controls="acctest3" type="button" class="btn btn-link collapsed" aria-expanded="false">1</button></h2></div><div data-parent="#acctest1" id="acctest3" aria-labelledby="acctest2" class="collapse"><div class="card-body">filler</div></div></div><div class="card"><div id="acctest4" class="card-header"><h2 class="mb-0"><button data-toggle="collapse" data-target="#acctest5" aria-controls="acctest5" type="button" class="btn btn-link collapsed" aria-expanded="false">2</button></h2></div><div data-parent="#acctest1" id="acctest5" aria-labelledby="acctest4" class="collapse"><div class="card-body">filler</div></div></div><div class="card"><div id="acctest6" class="card-header"><h2 class="mb-0"><button data-toggle="collapse" data-target="#acctest7" aria-controls="acctest7" type="button" class="btn btn-link collapsed" aria-expanded="false">3</button></h2></div><div data-parent="#acctest1" id="acctest7" aria-labelledby="acctest6" class="collapse"><div class="card-body">filler</div></div></div></div>
|]
