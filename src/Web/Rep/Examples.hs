{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Web.Rep.Examples
  ( page1,
    page2,
    cfg2,
    RepExamples (..),
    repExamples,
    Shape (..),
    fromShape,
    toShape,
    SumTypeExample (..),
    repSumTypeExample,
    SumType2Example (..),
    repSumType2Example,
    listExample,
    listRepExample,
    fiddleExample,
  )
where

import Data.Biapplicative
import Data.Bool
import MarkupParse.FlatParse
import MarkupParse
import Data.String.Interpolate
import Data.Text (pack)
import GHC.Generics
import Optics.Core
import Web.Rep
import Data.ByteString (ByteString)
import Data.Tree
import FlatParse.Basic (strToUtf8, takeRest)

-- | simple page example
page1 :: Page
page1 =
  #htmlBody .~ button1 $
    #cssBody .~ css1 $
      #jsGlobal .~ mempty $
        #jsOnLoad .~ click $
          #libsCss .~ (libCss <$> cssLibs) $
            #libsJs .~ (libJs <$> jsLibs) $
              mempty

-- | page with localised libraries
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
            defaultPageConfig ""

cssLibs :: [ByteString]
cssLibs =
  ["http://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css"]

cssLibsLocal :: [ByteString]
cssLibsLocal = ["css/font-awesome.min.css"]

jsLibs :: [ByteString]
jsLibs = ["http://code.jquery.com/jquery-1.6.3.min.js"]

jsLibsLocal :: [ByteString]
jsLibsLocal = ["jquery-2.1.3.min.js"]

css1 :: Css
css1 =
  Css
    [i|
{
  font-size   : 10px;
  font-family : "Arial","Helvetica", sans-serif;
}

\#btnGo
{
  margin-top    : 20px;
  margin-bottom : 20px;
}

\#btnGo.on
{
  color : \#008000;
}
|]

-- js
click :: Js
click =
  Js
    [i|
$('\#btnGo').click( function() {
  $('\#btnGo').toggleClass('on');
  alert('bada bing!');
});
|]

button1 :: [Tree Token]
button1 =
  pure $ wrap "button"
    [Attr "id" "btnGo",
     Attr "type" "button"]
    [ pure (Content "Go"), pure $ tag "i" [Attr "class" "fa fa-play"]]

-- | One of each sharedrep input instances.
data RepExamples = RepExamples
  { repTextbox :: ByteString,
    repTextarea :: ByteString,
    repSliderI :: Int,
    repSlider :: Double,
    repSliderVI :: Int,
    repSliderV :: Double,
    repCheckbox :: Bool,
    repToggle :: Bool,
    repDropdown :: Int,
    repDropdownMultiple :: [Int],
    repShape :: Shape,
    repColor :: ByteString
  }
  deriving (Show, Eq, Generic)

-- | For a typed dropdown example.
data Shape = SquareShape | CircleShape deriving (Eq, Show, Generic)

-- | shape parser
toShape :: ByteString -> Shape
toShape t = case t of
  "Circle" -> CircleShape
  "Square" -> SquareShape
  _ -> CircleShape

-- | shape printer
fromShape :: Shape -> ByteString
fromShape CircleShape = "Circle"
fromShape SquareShape = "Square"

-- | one of each input SharedReps
repExamples :: (Monad m) => SharedRep m RepExamples
repExamples = do
  t <- textbox (Just "textbox") "sometext"
  ta <- textarea 3 (Just "textarea") "no initial value & multi-line text\\nrenders is not ok?/"
  n <- sliderI (Just "int slider") 0 5 1 3
  ds' <- slider (Just "double slider") 0 1 0.1 0.5
  nV <- sliderVI (Just "int slider") 0 5 1 3
  dsV' <- sliderV (Just "double slider") 0 1 0.1 0.5
  c <- checkbox (Just "checkbox") True
  tog <- toggle (Just "toggle") False
  dr <- dropdown int (strToUtf8 . show) (Just "dropdown") (strToUtf8 . show <$> [1 .. 5 :: Int]) 3
  drm <- dropdownMultiple int (strToUtf8 . show) (Just "dropdown multiple") (strToUtf8 . show <$> [1 .. 5 :: Int]) [2, 4]
  drt <- toShape <$> dropdown takeRest id (Just "shape") ["Circle", "Square"] (fromShape SquareShape)
  col <- colorPicker (Just "color") "#454e56"
  pure (RepExamples t ta n ds' nV dsV' c tog dr drm drt col)

listExample :: (Monad m) => Int -> SharedRep m [Int]
listExample n =
  accordionList
    (Just "accordianListify")
    "al"
    Nothing
    (\l a -> sliderI (Just l) (0 :: Int) n 1 a)
    ((\x -> "[" <> (strToUtf8 . show) x <> "]") <$> [0 .. n] :: [ByteString])
    [0 .. n]

listRepExample :: (Monad m) => Int -> SharedRep m [Int]
listRepExample n =
  listRep
    (Just "listifyMaybe")
    "alm"
    (checkbox Nothing)
    (sliderI Nothing (0 :: Int) n 1)
    n
    3
    [0 .. 4]

fiddleExample :: Concerns ByteString
fiddleExample =
  Concerns
    mempty
    mempty
    [i|
<div class=" form-group-sm "><label for="1">fiddle example</label><input max="10.0" value="3.0" oninput="jsb.event({ &\#39;element&\#39;: this.id, &\#39;value&\#39;: this.value});" step="1.0" min="0.0" id="1" type="range" class=" custom-range  form-control-range "></div>
|]

data SumTypeExample = SumInt Int | SumOnly | SumText ByteString deriving (Eq, Show, Generic)

sumTypeText :: SumTypeExample -> ByteString
sumTypeText (SumInt _) = "SumInt"
sumTypeText SumOnly = "SumOnly"
sumTypeText (SumText _) = "SumText"

repSumTypeExample :: (Monad m) => Int -> ByteString -> SumTypeExample -> SharedRep m SumTypeExample
repSumTypeExample defi deft defst =
  bimap hmap mmap repst <<*>> repi <<*>> rept
  where
    repi = sliderI Nothing 0 20 1 defInt
    rept = textbox Nothing defText
    repst =
      dropdownSum
        takeRest
        id
        (Just "SumTypeExample")
        ["SumInt", "SumOnly", "SumText"]
        (sumTypeText defst)
    hmap repst' repi' rept' =
      wrap "div"
        ( repst'
            <> tag repi'
              [ Attr "class" "subtype ",
                Attr "data-sumtype" "SumInt",
                Attr "style"
                  ( "display:"
                      <> bool "block" "none" (sumTypeText defst /= "SumInt")
                  )
              ]
            <> with
              rept'
              [ Attr "class" "subtype ",
                Attr "data-sumtype" "SumText",
                Attr "style"
                  ("display:" <> bool "block" "none" (sumTypeText defst /= "SumText"))
              ]
        )
    mmap repst' repi' rept' =
      case repst' of
        "SumInt" -> SumInt repi'
        "SumOnly" -> SumOnly
        "SumText" -> SumText rept'
        _ -> SumOnly
    defInt = case defst of
      SumInt i -> i
      _NotSumInt -> defi
    defText = case defst of
      SumText t -> t
      _NotSumText -> deft

data SumType2Example = SumOutside Int | SumInside SumTypeExample deriving (Eq, Show, Generic)

sumType2Text :: SumType2Example -> ByteString
sumType2Text (SumOutside _) = "SumOutside"
sumType2Text (SumInside _) = "SumInside"

repSumType2Example :: (Monad m) => Int -> ByteString -> SumTypeExample -> SumType2Example -> SharedRep m SumType2Example
repSumType2Example defi deft defst defst2 =
  bimap hmap mmap repst2 <<*>> repst <<*>> repoi
  where
    repoi = sliderI Nothing 0 20 1 defInt
    repst = repSumTypeExample defi deft SumOnly
    repst2 =
      dropdownSum
        takeRest
        id
        (Just "SumType2Example")
        ["SumOutside", "SumInside"]
        (sumType2Text defst2)
    hmap repst2' repst' repoi' =
      wrap "div" []
        ( repst2'
            <> tag repst'
              [ Attr "class" "subtype ",
                Attr "data-sumtype" "SumInside",
                Attr "style"
                  ( "display:"
                      <> bool "block" "none" (sumType2Text defst2 /= "SumInside")
                  )
              ]
            <> pure (tag repoi'
              [ Attr "class" "subtype ",
                Attr "data-sumtype" "SumOutside",
                Attr "style"
                  ( "display:"
                      <> bool "block" "none" (sumType2Text defst2 /= "SumOutside")
                  )
              ])
        )
    mmap repst2' repst' repoi' =
      case repst2' of
        "SumOutside" -> SumOutside repoi'
        "SumInside" -> SumInside repst'
        _WeirdSpelling -> SumOutside repoi'
    defInt = case defst of
      SumInt i -> i
      _NotSumInt -> defi
