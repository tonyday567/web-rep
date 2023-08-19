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
    listExample,
    listRepExample,
  )
where

import MarkupParse.FlatParse
import MarkupParse
import Data.String.Interpolate
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

