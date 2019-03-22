{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Test.Tasty
import           Test.Tasty.Hspec
import Protolude
import Lens.Micro
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Text
import Web.Page
import Clay hiding (pc, dir, p, q)
import Lucid
import Text.InterpolatedString.Perl6

page1 :: Page
page1 =
  #htmlBody .~ Main.button $
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
  #localdirs .~ ["static"] $
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
  fontSize (px 10)
  fontFamily ["Arial", "Helvetica"] [sansSerif]
  "#btnGo" ? do
    marginTop (px 20)
    marginBottom (px 20)
  "#btnGo.on" ? color green

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

generatePage :: FilePath -> FilePath -> PageConfig -> Page -> IO ()
generatePage dir stem pc =
  renderPageToFile dir (#filenames .~ concernNames "" stem $ pc) 

generatePages ::
     Traversable t => FilePath -> t (FilePath, PageConfig, Page) -> IO ()
generatePages dir xs =
  void $ sequenceA $ (\(fp, pc, p) -> generatePage dir fp pc p) <$> xs

genTest :: FilePath -> IO ()
genTest dir =
  void $ generatePages dir [("default", defaultPageConfig, page1), ("sep", cfg2, page2)]

testVsFile :: FilePath -> FilePath -> PageConfig -> Page -> IO Bool
testVsFile dir stem pc p = do
  (t,t') <- textVsFile dir stem pc p
  pure (t==t')

textVsFile
  :: FilePath
  -> FilePath
  -> PageConfig
  -> Page
  -> IO (Concerns Lazy.Text, Concerns Lazy.Text)
textVsFile dir stem pc p = do
  let names = concernNames "" stem
  let pc' = #filenames .~ names $ pc
  let t = renderPageAsText pc' p
  case pc ^. #concerns of
    Inline -> do
      t' <- Text.readFile (dir <> names ^. #html)
      return (t, Concerns mempty mempty t')
    Separated -> do
      t' <- sequenceA $ Text.readFile <$> (dir <>) <$> names
      return (t, t')

testsRender :: IO (SpecWith ())
testsRender =
  return $
    describe "Web.Page.Render" $ do
      it "run genTest 'test/canned/' to refresh canned files." True
      it "renderPage mempty" $
        renderText (renderPage mempty) `shouldBe`
        "<!DOCTYPE HTML><html lang=\"en\"><head><meta charset=\"utf-8\"></head><body><script>window.onload=function(){}</script></body></html>"
      let dir = "test/canned/"
      it "renderPageToFile, renderPage (compared with default canned file)" $
        testVsFile dir "default" defaultPageConfig page1 `shouldReturn` True
      it "the various PageConfig's" $
        testVsFile dir "sep" cfg2 page2 `shouldReturn` True
 
-- The tests
tests :: IO TestTree
tests = testGroup "the tests" <$> sequence
  [ testSpec "Web.Page.Render" =<< testsRender
  ]

main :: IO ()
main = defaultMain =<< tests
