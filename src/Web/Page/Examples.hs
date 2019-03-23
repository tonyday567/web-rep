{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Web.Page.Examples where

import Web.Page
import Text.InterpolatedString.Perl6
import Protolude
import qualified Clay
import Lucid
import Lens.Micro

-- | simple page examples
page1 :: Page
page1 =
  #htmlBody .~ button $
  #cssBody .~ css $
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

css :: Css
css = do
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

button :: Html ()
button =
  with
    button_
    [id_ "btnGo", Lucid.type_ "button"]
    ("Go " <> with i_ [class_ "fa fa-play"] mempty)
