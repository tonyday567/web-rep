{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module TestRender where

import Web.Page

import Protolude
import Control.Lens
import Data.Default
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Text
import Network.HTTP.Client
import Pipes
import qualified Pipes.ByteString as PB -- from `pipes-bytestring`
import Pipes.HTTP
import Test.Tasty.Hspec

page1 :: Page
page1 =
  #htmlBody .~ TestRender.button $
  #cssBody .~ css $
  #jsGlobal .~ mempty $
  #jsOnLoad .~ click $
  #libsCss .~ cssLibs $
  #libsJs .~ jsLibs $
  mempty

page2 :: Page
page2 =
  #libsCss .~ cssLibsLocal $
  #libsJs .~ jsLibsLocal $
  page1

pagecfg2 :: PageConfig
pagecfg2 =
  #concerns .~ Separated $
  #pageRender .~ Pretty $
  #structure .~ Headless $
  #pageLibs .~ LocalLibs "../static/" $
  def

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
click :: [JSStatement]
click = toStatements $ JS $ readJs $
  unlines
  [ "$('#btnGo').click( function() {"
  , "$('#btnGo').toggleClass('on');"
  , "alert('bada bing!');"
   , "});"
  ]

button :: Html ()
button =
  with
    button_
    [id_ "btnGo", type_ "button"]
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
  void $ generatePages dir [("default", def, page1), ("sep", pagecfg2, page2)]

testVsFile :: FilePath -> FilePath -> PageConfig -> Page -> IO Bool
testVsFile dir stem pc p = do
  (t,t') <- textVsFile dir stem pc p
  pure (t==t')

textVsFile ::
     FilePath
  -> FilePath
  -> PageConfig
  -> Page
  -> IO (Concerns Lazy.Text, Concerns Lazy.Text)
textVsFile dir stem pc p = do
  let names = concernNames "" stem
  let pc' = #filenames .~ names $ pc
  let t = renderPageText pc' p
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
        "<html><head><meta charset=\"utf-8\"><script>window.onload=function(){}</script><body></body></head></html>"
    -- it "renderPageToFile, renderPage (compared with default canned file)" $
    --   testVsFile dir "default" def page1 `shouldReturn` True
    -- it "the various PageConfig's" $
    --   testVsFile dir "sep" pagecfg2 page2 `shouldReturn` True

-- testServe :: PageConfig -> Page -> IO ()
testServe :: PageConfig -> Page -> IO ()
testServe pc p = serve' (servePageWith pc p)

pServe :: Page
pServe = #libsCss .~ cssLibsLocal $ #libsJs .~ jsLibsLocal $ page1

pcServe :: PageConfig
pcServe =
  #concerns .~ Separated $
  #pageRender .~ Pretty $
  #structure .~ Headless $
  #pageLibs .~ LocalLibs "static/" $
  #filenames .~
  Concerns "static/page-serve.css" "static/page-serve.js" mempty $
  def

testRequest :: Pipes.HTTP.Request
testRequest = defaultRequest {Pipes.HTTP.method = "GET", Pipes.HTTP.port = 8001}

testResponse :: Manager -> (Pipes.HTTP.Response BodyReader -> IO a) -> IO a
testResponse = withResponse testRequest

client :: IO ()
client = do
  m <- newManager tlsManagerSettings
  withHTTP testRequest m $ \resp ->
      Pipes.runEffect $ responseBody resp >-> PB.stdout

tReq :: IO Pipes.HTTP.Request
tReq = parseRequest "0.0.0.0:8001"
