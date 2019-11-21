{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}

module Web.Page.Examples
  ( page1
  , page2
  , cfg2
  , RepExamples(..)
  , repExamples
  , Shape(..)
  , fromShape
  , toShape
  , SumTypeExample(..)
  , repSumTypeExample
  , SumType2Example(..)
  , repSumType2Example
  , listExample
  , listRepExample
  , fiddleExample
  ) where

import Control.Lens hiding ((.=))
import Data.Attoparsec.Text
import Lucid
import Prelude
import Web.Page
import qualified Clay
import GHC.Generics

-- | simple page examples
page1 :: Page
page1 =
  #htmlBody .~ button1 $
  #cssBody .~ PageCss css1 $
  #jsGlobal .~ mempty $
  #jsOnLoad .~ click $
  #libsCss .~ (libCss <$> cssLibs) $
  #libsJs .~ (libJs <$> jsLibs) $
  mempty

page2 :: Page
page2 =
  #libsCss .~ (libCss <$> cssLibsLocal) $
  #libsJs .~ (libJs <$> jsLibsLocal) $
  page1

cfg2 :: PageConfig
cfg2 =
  #concerns .~ Separated $
  #pageRender .~ Pretty $
  #structure .~ Headless $
  #localdirs .~ ["test/static"] $
  #filenames .~ (("other/cfg2" <>) <$> suffixes) $
  (defaultPageConfig "")

cssLibs :: [Text]
cssLibs =
  ["http://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css"]

cssLibsLocal :: [Text]
cssLibsLocal = ["css/font-awesome.min.css"]

jsLibs :: [Text]
jsLibs = ["http://code.jquery.com/jquery-1.6.3.min.js"]

jsLibsLocal :: [Text]
jsLibsLocal = ["jquery-2.1.3.min.js"]

css1 :: Css
css1 = do
  Clay.fontSize (Clay.px 10)
  Clay.fontFamily ["Arial", "Helvetica"] [Clay.sansSerif]
  "#btnGo" Clay.? do
    Clay.marginTop (Clay.px 20)
    Clay.marginBottom (Clay.px 20)
  "#btnGo.on" Clay.? Clay.color Clay.green

-- js
click :: PageJs
click = PageJsText [q|
$('#btnGo').click( function() {
  $('#btnGo').toggleClass('on');
  alert('bada bing!');
});
|]

button1 :: Html ()
button1 =
  with
    button_
    [id_ "btnGo", Lucid.type_ "button"]
    ("Go " <> with i_ [class__ "fa fa-play"] mempty)

data RepExamples =
  RepExamples
  { repTextbox :: Text
  , repTextarea :: Text
  , repSliderI :: Int
  , repSlider :: Double
  , repCheckbox :: Bool
  , repToggle :: Bool
  , repDropdown :: Int
  , repShape :: Shape
  , repColor :: PixelRGB8
  } deriving (Show, Eq, Generic)

data Shape = SquareShape | CircleShape deriving (Eq, Show, Generic)

toShape :: Text -> Shape
toShape t = case t of
  "Circle" -> CircleShape
  "Square" -> SquareShape
  _ -> CircleShape

fromShape :: Shape -> Text
fromShape CircleShape = "Circle"
fromShape SquareShape = "Square"

repExamples :: (Monad m) => SharedRep m RepExamples
repExamples = do
  t <- textbox (Just "textbox") "sometext"
  ta <- textarea 3 (Just "textarea") "no initial value & multi-line text\\nrenders is not ok?/"
  n <- sliderI (Just "int slider") 0 5 1 3
  ds' <- slider (Just "double slider") 0 1 0.1 0.5
  c <- checkbox (Just "checkbox") True
  tog <- toggle (Just "toggle") False
  dr <- dropdown decimal (pack . show) (Just "dropdown") ((pack . show) <$> [1..5::Int]) 3
  drt <- toShape <$> dropdown takeText id (Just "shape") (["Circle", "Square"]) (fromShape SquareShape)
  col <- colorPicker (Just "color") (PixelRGB8 56 128 200)
  pure (RepExamples t ta n ds' c tog dr drt col)

-- encodeFile "saves/rep2.json" $ RepExamples "text1" "text2" 1 1.0 True True 2 (PixelRGB8 0 100 0)
-- decodeFileStrict "saves/rep2.json" :: IO (Maybe RepExamples)

listExample :: (Monad m) => Int -> SharedRep m [Int]
listExample n =
  accordionList (Just "accordianListify") "al" Nothing
  (\l a -> sliderI (Just l) (0::Int) n 1 a) ((\x -> "[" <> (pack . show) x <> "]") <$> [0..n] :: [Text]) [0..n]

listRepExample :: (Monad m) => Int -> SharedRep m [Int]
listRepExample n =
  listRep (Just "listifyMaybe") "alm" (checkbox Nothing)
    (sliderI Nothing (0::Int) n 1) n 3 [0..4]

fiddleExample :: Concerns Text
fiddleExample = Concerns mempty mempty
  [q|
<div class=" form-group-sm "><label for="1">fiddle example</label><input max="10.0" value="3.0" oninput="jsb.event({ &#39;element&#39;: this.id, &#39;value&#39;: this.value});" step="1.0" min="0.0" id="1" type="range" class=" custom-range  form-control-range "></div>
|]

data SumTypeExample = SumInt Int | SumOnly | SumText Text deriving (Eq, Show, Generic)

sumTypeText :: SumTypeExample -> Text
sumTypeText (SumInt _) = "SumInt"
sumTypeText SumOnly = "SumOnly"
sumTypeText (SumText _) = "SumText"

repSumTypeExample :: (Monad m) => Int -> Text -> SumTypeExample -> SharedRep m SumTypeExample
repSumTypeExample defi deft defst =
  bimap hmap mmap repst <<*>> repi <<*>> rept
  where
    repi = sliderI Nothing 0 20 1 defInt
    rept = textbox Nothing defText
    repst = dropdownSum takeText id (Just "SumTypeExample")
      ["SumInt", "SumOnly", "SumText"]
      (sumTypeText defst)
    hmap repst' repi' rept' =
      div_
      (repst' <>
      with repi'
        [ class__ "subtype "
        , data_ "sumtype" "SumInt"
        , style_
          ("display:" <>
           bool "block" "none" (sumTypeText defst /= "SumInt"))
        ] <>
      with rept'
        [ class__ "subtype "
        , data_ "sumtype" "SumText"
        , style_
          ("display:" <> bool "block" "none" (sumTypeText defst /= "SumText"))
        ])

    mmap repst' repi' rept' =
      case repst' of
        "SumInt" -> SumInt repi'
        "SumOnly" -> SumOnly
        "SumText" -> SumText rept'
        _ -> SumOnly

    defInt = case defst of
      SumInt i -> i
      _ -> defi
    defText = case defst of
      SumText t -> t
      _ -> deft

data SumType2Example = SumOutside Int | SumInside SumTypeExample deriving (Eq, Show, Generic)

sumType2Text :: SumType2Example -> Text
sumType2Text (SumOutside _) = "SumOutside"
sumType2Text (SumInside _) = "SumInside"

repSumType2Example :: (Monad m) => Int -> Text -> SumTypeExample -> SumType2Example -> SharedRep m SumType2Example
repSumType2Example defi deft defst defst2 =
  bimap hmap mmap repst2 <<*>> repst <<*>> repoi
  where
    repoi = sliderI Nothing 0 20 1 defInt
    repst = repSumTypeExample defi deft SumOnly
    repst2 = dropdownSum takeText id (Just "SumType2Example")
      ["SumOutside", "SumInside"]
      (sumType2Text defst2)
    hmap repst2' repst' repoi' =
      div_
      (repst2' <>
      with repst'
        [ class__ "subtype "
        , data_ "sumtype" "SumInside"
        , style_
          ("display:" <>
           bool "block" "none" (sumType2Text defst2 /= "SumInside"))
        ] <>
      with repoi'
        [ class__ "subtype "
        , data_ "sumtype" "SumOutside"
        , style_
          ("display:" <>
           bool "block" "none" (sumType2Text defst2 /= "SumOutside"))
        ])

    mmap repst2' repst' repoi' =
      case repst2' of
        "SumOutside" -> SumOutside repoi'
        "SumInside" -> SumInside repst'
        _ -> SumOutside repoi'

    defInt = case defst of
      SumInt i -> i
      _ -> defi
