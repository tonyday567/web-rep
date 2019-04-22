{-# LANGUAGE ApplicativeDo #-}
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
  , SumTypeExample(..)
  , repSumTypeExample
  , listifyExample
  , fiddleExample
  ) where

import Control.Category (id)
import Control.Lens
import Data.Attoparsec.Text
import Lucid
import Protolude
import Web.Page
import qualified Clay

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
  col <- colorPicker "color" (PixelRGB8 56 128 200)
  pure (RepExamples t ta n ds c tog dr col)

listifyExample :: (Monad m) => Int -> SharedRep m [Int]
listifyExample n =
  accordionListify (Just "accordianListify") "al" Nothing
  (\l a -> sliderI l (0::Int) n 1 a) ((\x -> "[" <> show x <> "]") <$> [0..n] :: [Text]) [0..n]

fiddleExample :: Concerns Text
fiddleExample = Concerns mempty mempty
  [q|
<div class=" form-group-sm "><label for="1">fiddle example</label><input max="10.0" value="3.0" oninput="jsb.event({ &#39;element&#39;: this.id, &#39;value&#39;: this.value});" step="1.0" min="0.0" id="1" type="range" class=" custom-range  form-control-range "></div>
|]

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
              with hi [ class__ "subtype "
                      , data_ "sumtype" "SumInt"
                      , style_
                   ("display:" <> bool "block" "none" (sumTypeText defst /= "SumInt"))] <>
              with ht [ class__ "subtype "
                      , data_ "sumtype" "SumText"
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
