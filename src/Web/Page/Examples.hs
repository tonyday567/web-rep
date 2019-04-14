{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Web.Page.Examples where

import Control.Lens
import Data.Attoparsec.Text
import Lucid
import Protolude
import Text.InterpolatedString.Perl6
import Web.Page
import Web.Page.Html
import Web.Page.Rep
import qualified Clay

-- | simple page examples
page1 :: Page
page1 =
  #htmlBody .~ button1 $
  #cssBody .~ css1 $
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
  defaultPageConfig

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
    ("Go " <> with i_ [class_ "fa fa-play"] mempty)

data RepExamples =
  RepExamples
  { repTextbox :: Text
  , repSliderI :: Int
  , repSlider :: Double
  , repCheckbox :: Bool
  , repToggle :: Bool
  , repDropdown :: Int
  , repColor :: PixelRGB8
  } deriving (Show, Eq)

repExamples :: (Monad m) => SharedRep m RepExamples
repExamples = do
  t <- textbox "textbox" "sometext"
  n <- sliderI "int slider" 0 5 1 3
  ds <- slider "double slider" 0 1 0.1 0.5
  c <- checkbox "checkbox" True
  tog <- toggle "toggle" False
  dr <- dropdown decimal show "dropdown" (show <$> [1..5::Int]) 3
  col <- color "color" (PixelRGB8 56 128 200)
  pure (RepExamples t n ds c tog dr col)

listifyExample :: (Monad m) => SharedRep m [Int]
listifyExample = listify (\l a -> sliderI l (0::Int) 10 1 a) (show <$> [0..10::Int] :: [Text]) [0..10]
