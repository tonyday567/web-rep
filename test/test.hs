{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Control.Lens
import Lucid
import NumHask.Prelude
import Test.DocTest
import Test.Tasty
import Test.Tasty.Hspec
import Web.Rep
import Web.Rep.Examples
import qualified Data.Text.IO as Text

generatePage :: FilePath -> FilePath -> PageConfig -> Page -> IO ()
generatePage dir stem pc =
  renderPageToFile dir (#filenames .~ concernNames "" stem $ pc) 

generatePages ::
     Traversable t => FilePath -> t (FilePath, PageConfig, Page) -> IO ()
generatePages dir xs =
  sequenceA_ $ (\(fp, pc, p) -> generatePage dir fp pc p) <$> xs

genTest :: FilePath -> IO ()
genTest dir =
  void $ generatePages dir [("default", defaultPageConfig "default", page1), ("sep", cfg2, page2)]

testVsFile :: FilePath -> FilePath -> PageConfig -> Page -> IO Bool
testVsFile dir stem pc p = do
  (t,t') <- textVsFile dir stem pc p
  pure (t==t')

textVsFile
  :: FilePath
  -> FilePath
  -> PageConfig
  -> Page
  -> IO (Concerns Text, Concerns Text)
textVsFile dir stem pc p = do
  let names = concernNames "" stem
  let pc' = #filenames .~ names $ pc
  let t = renderPageAsText pc' p
  case pc ^. #concerns of
    Inline -> do
      t' <- Text.readFile (dir <> names ^. #htmlConcern)
      return (t, Concerns mempty mempty t')
    Separated -> do
      t' <- sequenceA $ Text.readFile . (dir <>) <$> names
      return (t, t')

testsRender :: IO (SpecWith ())
testsRender =
  return $
    describe "Web.Rep.Render" $ do
      it "run genTest 'test/canned/' to refresh canned files." True
      it "renderPage mempty" $
        renderText (renderPage mempty) `shouldBe`
        "<!DOCTYPE HTML><html lang=\"en\"><head><meta charset=\"utf-8\"></head><body><script>window.onload=function(){}</script></body></html>"
      let dir = "test/canned/"
      it "renderPageToFile, renderPage (compared with default canned file)" $
        testVsFile dir "default" (defaultPageConfig "default") page1 `shouldReturn` True
      it "the various PageConfig's" $
        testVsFile dir "sep" cfg2 page2 `shouldReturn` True

testsBootstrap :: IO (SpecWith ())
testsBootstrap =
  return $
    describe "Web.Rep.Bootstrap" $ do
      it "bootstrapPage versus canned" $
        toText (renderPage bootstrapPage) `shouldBe`
        "<!DOCTYPE HTML><html lang=\"en\"><head><meta charset=\"utf-8\"><link crossorigin=\"anonymous\" href=\"https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css\" integrity=\"sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T\" rel=\"stylesheet\"><meta charset=\"utf-8\"><meta content=\"width=device-width, initial-scale=1, shrink-to-fit=no\" name=\"viewport\"></head><body><script crossorigin=\"anonymous\" integrity=\"sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo\" src=\"https://code.jquery.com/jquery-3.3.1.slim.min.js\"></script><script crossorigin=\"anonymous\" integrity=\"sha384-UO2eT0CpHqdSJQ6hJty5KVphtPhzWj9WO1clHTMGa3JDZwrnQq4sF86dIHNDz0W1\" src=\"https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js\"></script><script crossorigin=\"anonymous\" integrity=\"sha384-JjSmVgyd0p3pXB1rRibZUAYoIIy6OrQ6VrjIEaFf/nJGzIxFDsf4x0xIM+B07jRM\" src=\"https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js\"></script><script>window.onload=function(){}</script></body></html>"
      it "accordion versus canned" $
        ( toText .
          runIdentity .
          flip evalStateT 0 .
          accordion "acctest" Nothing $
          (\x -> (pack (show x), "filler")) <$> [1..2::Int]) `shouldBe`
        "<div id=\"acctest1\" class=\" accordion m-1 \"><div class=\" card \"><div id=\"acctest2\" class=\" card-header p-0 \"><h2 class=\" m-0 \"><button data-toggle=\"collapse\" data-target=\"#acctest3\" aria-controls=\"acctest3\" type=\"button\" class=\" btn btn-link collapsed \" aria-expanded=\"false\">1</button></h2></div><div data-parent=\"#acctest1\" id=\"acctest3\" aria-labelledby=\"acctest2\" class=\" collapse \"><div class=\" card-body \">filler</div></div></div><div class=\" card \"><div id=\"acctest4\" class=\" card-header p-0 \"><h2 class=\" m-0 \"><button data-toggle=\"collapse\" data-target=\"#acctest5\" aria-controls=\"acctest5\" type=\"button\" class=\" btn btn-link collapsed \" aria-expanded=\"false\">2</button></h2></div><div data-parent=\"#acctest1\" id=\"acctest5\" aria-labelledby=\"acctest4\" class=\" collapse \"><div class=\" card-body \">filler</div></div></div></div>"

-- The tests
tests :: IO TestTree
tests = testGroup "the tests" <$> sequence
  [ testSpec "Web.Rep.Render" =<< testsRender
  , testSpec "Web.Rep.Bootstrap" =<< testsBootstrap
  ]

main :: IO ()
main = do
  doctest ["src/Web/Rep/SharedReps.hs"]
  defaultMain =<< tests
