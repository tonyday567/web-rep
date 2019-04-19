{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
 
module Web.Page.Examples where

import Control.Category (id)
import Control.Lens
import Data.Attoparsec.Text
import Lucid
import Protolude
import Text.InterpolatedString.Perl6
import Web.Page
import Web.Page.Css ()
import Web.Page.Html
import Web.Page.Rep
import qualified Clay
-- import qualified Data.Text.Lazy as Lazy

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
  , repTextarea :: Text
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
  ta <- textarea 3 "textarea" "no initial value & multi-line text\\nrenders is not ok?/"
  n <- sliderI "int slider" 0 5 1 3
  ds <- slider "double slider" 0 1 0.1 0.5
  c <- checkbox "checkbox" True
  tog <- toggle "toggle" False
  dr <- dropdown decimal show "dropdown" (show <$> [1..5::Int]) 3
  col <- color "color" (PixelRGB8 56 128 200)
  pure (RepExamples t ta n ds c tog dr col)

listifyExample :: (Monad m) => SharedRep m [Int]
listifyExample = listify (\l a -> sliderI l (0::Int) 10 1 a) (show <$> [0..10::Int] :: [Text]) [0..10]

fiddleExample :: Concerns Text
fiddleExample = Concerns
  [q|
.menu{
    margin:20px;
    }
|]
    [q|
$(".dropdown-menu li a").click(function(){
  var selText = $(this).attr('data-value');
    $(this).parents('.btn-group').siblings('.menu').html(selText)
});
      |]
      [q|
<form>
   <div class="btn-group">
      <a class="btn dropdown-toggle btn-select" data-toggle="dropdown" href="#">Select a Items <span class="caret"></span></a>
      <ul class="dropdown-menu">
        <li><a href="#" data-value="action 1">Item I</a></li>
        <li><a href="#" data-value="action 2">Item II</a></li>
        <li><a href="#" data-value="action 3">Item III</a></li>
      </ul>
   </div>
   <p class="menu">Options:</p>
</form>
|]


fiddleExampleDev :: Int -> Concerns Text
fiddleExampleDev n = Concerns
  [q|
|]
    [q|
$(".dropdown-menu li a").click(function(){
  var selText = $(this).attr('data-value');
    $(this).parents('.btn-group').siblings('.menu').html(selText)
});
      |]
      ([q|
<form>
   <div class="btn-group">
      <button class="btn btn-secondary dropdown-toggle btn-select" data-toggle="dropdown" href="#" aria-haspopup="true" aria-expanded="false" id="d1">Sum Type</button>
      <ul class="dropdown-menu" aria-labelledby="d1">
|]
          <> mconcat ((\x -> [qc| <li><a href="#" data-value="action {show x :: Text}">Item {show x :: Text}</a></li>|]) <$> [1..n]) <>
         [q|
           </ul>
   </div>
   <p class="menu">Options:</p>
</form>
|])


data SumTypeExample = SumInt Int | SumOnly | SumText Text deriving (Eq, Show)

sumTypeText :: SumTypeExample -> Text
sumTypeText (SumInt _) = "SumInt"
sumTypeText SumOnly = "SumOnly"
sumTypeText (SumText _) = "SumText"

repSumTypeExample :: (Monad m) => Int -> Text -> SumTypeExample -> SharedRep m SumTypeExample
repSumTypeExample defi deft defst = SharedRep $ do
  (Rep hi fi) <- unrep $ sliderI "" 0 20 1 defInt
  (Rep ht ft) <- unrep $ textbox "" defText
  (Rep hdb fdb) <- unrep $ dropdownSum takeText id "SumTypeExample"
    ["SumInt", "SumOnly", "SumText"]
    (sumTypeText defst)
  pure $ Rep (hdb <>
              with hi [ data_ "sumtype" "SumInt"
                      , style_
                   ("display:" <> bool "block" "none" (sumTypeText defst /= "SumInt"))] <>
              with ht [ data_ "sumtype" "SumText"
                      , style_
                   ("display:" <> bool "block" "none" (sumTypeText defst /= "SumText"))])
    (\m -> let (m', db) = fdb m in
            case db of
              Left e -> (m', Left e)
              Right "SumInt" -> second (second SumInt) (fi m')
              Right "SumOnly" -> (m', Right SumOnly)
              Right "SumText" -> second (second SumText) (ft m')
              Right _ -> (m', Left "bad sumtype text"))
    where
      defInt = case defst of
        SumInt i -> i
        _ -> defi
      defText = case defst of
        SumText t -> t
        _ -> deft


