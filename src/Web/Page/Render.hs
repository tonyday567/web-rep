{-# LANGUAGE OverloadedStrings #-}

module Web.Page.Render 
  ( renderPage
  , renderPageWith
  , renderPageHtmlWith
  , renderPageText
  , renderPageToFile
  , renderPageHtmlToFile
  ) where

import           Control.Applicative
import           Control.Lens
import Control.Monad
import           Data.Default
import           Data.Monoid
import           Data.Text.Lazy (Text, pack, toStrict, fromStrict)
import           Data.Text.Lazy.IO (writeFile)
import qualified Data.Text as Text
import Data.Traversable
import           Lucid
import           Prelude hiding (writeFile)
import qualified Web.Page.Css as Css
import qualified Web.Page.Html as Html
import qualified Web.Page.Js as Js
import           Web.Page.Types
-- import qualified Text.Blaze.Html5 as Html5

renderPage :: Page -> Html ()
renderPage p = (\(_,_,x)->x) $ renderPageWith (pagecConcerns .~ Inline $ def) p

renderPageHtmlWith :: PageConfig -> Page -> Html ()
renderPageHtmlWith pc p = (\(_,_,x)->x) $ renderPageWith (pagecConcerns .~ Inline $ pc) p

renderPageWith :: PageConfig -> Page -> (Text, Text, Html ())
renderPageWith pc p =
  case pc^.pagecConcerns of
    Inline    -> (mempty, mempty, h)
    Separated -> (css, js, h)
  where
    h = case pc^.pagecStructure of
      HeaderBody -> 
        html_ $
        head_ $ mconcat
        [ meta_ [charset_ "utf-8"]
        , mconcat (((\x -> link_ [x,rel_ "stylesheet"]) . href_) <$> libsCss')
        , mconcat ((\x -> with (script_ mempty) [src_ x]) <$> libsJs')
        , jsInline
        , cssInline
        , p^.pageHtmlHeader
        ] <>
        body_ (p^.pageHtmlBody)
      Headless ->
        mconcat
        [ doctype_
        , meta_ [charset_ "utf-8"]
        , mconcat (((\x -> link_ [x,rel_ "stylesheet"]) . href_) <$> libsCss')
        , cssInline
        , mconcat ((\x -> with (script_ mempty) [src_ x]) <$> libsJs')
        , p^.pageHtmlHeader
        , p^.pageHtmlBody
        , jsInline
        ]  
      Svg ->
        Html.doctypesvg_ <> 
        svg_ 
        (Html.defs_ $ mconcat
         [ renderLibsJsSvg (fromStrict <$> libsJs')
         , renderLibsCssSvg (fromStrict <$> libsCss')
         , cssInline
         , jsInline
         , p^.pageHtmlBody
         ])
    css = rendercss (p^.pageCss)
    js = renderjs (p^.pageJsGlobal <> Js.onLoad (p^.pageJsOnLoad))
    (renderjs, rendercss) = renderers $ pc^.pagecRender 
    cssInline
      | pc^.pagecConcerns == Separated || css==mempty = mempty
      | otherwise = style_ [type_ (toStrict $ pack "text/css")] css
    jsInline
      | pc^.pagecConcerns == Separated || js==mempty = mempty
      | otherwise = script_ mempty js
    libsCss = case pc^.pagecLibs of
      LinkedLibs -> p^.pageLibsCss
      LocalLibs dir -> (\x -> Text.pack dir <> x) <$> p^.pageLibsCss
    libsCss' = case pc^.pagecConcerns of
      Inline -> libsCss
      Separated -> libsCss <> [Text.pack (_css $ pc^.pagecFilenames)]
    libsJs = case pc^.pagecLibs of
      LinkedLibs -> p^.pageLibsJs
      LocalLibs dir -> (\x -> Text.pack dir <> x) <$> p^.pageLibsJs
    libsJs' = case pc^.pagecConcerns of
      Inline -> libsJs
      Separated -> libsJs <> [Text.pack (_js $ pc^.pagecFilenames)]

renderPageToFile :: FilePath -> PageConfig -> Page -> IO ()
renderPageToFile dir pc@(PageConfig _ _ _ _ files) page =
  void $ sequenceA $ liftA2 writeFile' files (renderPageText pc page)
  where
    writeFile' fp s = unless (s==mempty) (writeFile (dir<>"/"<>fp) s)

renderPageHtmlToFile :: FilePath -> PageConfig -> Page -> IO ()
renderPageHtmlToFile file pc page =
  writeFile file (renderText $ renderPageHtmlWith pc page)
  
renderPageText :: PageConfig -> Page -> Concerns Text
renderPageText pc p =
  case pc^.pagecConcerns of
    Inline    -> Concerns mempty mempty htmlt
    Separated -> Concerns css js htmlt
  where
    htmlt = renderText h
    (css,js,h) = renderPageWith pc p

renderLibsJsSvg :: [Text] -> Html ()
renderLibsJsSvg xs = mconcat $ libJsSvg <$> xs

libJsSvg :: Text -> Html ()
libJsSvg x =
  with (script_ mempty)
  [ href_ (toStrict x)
  , rel_ "stylesheet"
  , type_ (toStrict $ pack "text/css")
  , Html.makeAttribute "xmlns" "http://www.w3.org/1999/xhtml"
  ]

renderLibsCssSvg :: [Text] -> Html ()
renderLibsCssSvg xs =
  mconcat
  ((with Html.linkSvg_ .
    (\x ->
      [ href_ (toStrict x)
      , rel_ "stylesheet"
      , type_ (toStrict $ pack "text/css")
      , Html.makeAttribute "xmlns" "http://www.w3.org/1999/xhtml"
      ]
    )
   ) <$> xs) 
  mempty

renderers :: PageRender -> (Js.JStat -> Text, Css.Css -> Text)
renderers Minified  = (pack . Js.renderMin, Css.renderWith Css.compact []) 
renderers Pretty = (pack . Js.render, Css.render)
