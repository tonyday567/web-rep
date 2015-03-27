{-# LANGUAGE OverloadedStrings #-}

module Lucid.Page.Render 
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
import           Lucid
import           Prelude hiding (writeFile)
import qualified Lucid.Page.Css as Css
import qualified Lucid.Page.Html as Html
import qualified Lucid.Page.Js as Js
import           Lucid.Page.Types

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
    p' = case pc^.pagecConcerns of
      Inline -> p
      Separated -> pageLibsCss %~ (<>cssLink) $ pageLibsJs %~ (<>jsLink) $ p
    cssLink = if css == mempty then mempty else [Text.pack (_css $ pc^.pagecFilenames)]
    jsLink = if js == mempty then mempty else [Text.pack (_js $ pc^.pagecFilenames)]
    h = case pc^.pagecStructure of
      HeaderBody -> 
        doctypehtml_ $
        head_ $ mconcat
        [ meta_ [charset_ "utf-8"]
        , mconcat (((\x -> link_ [x,rel_ "stylesheet"]) . href_) <$> (p'^.pageLibsCss))
        , mconcat ((\x -> with (script_ mempty) [src_ x]) <$> p'^.pageLibsJs)
        , jsInline
        , cssInline
        , p'^.pageHtmlHeader
        ] <>
        body_ (p'^.pageHtmlBody)
      Headless ->
        mconcat
        [ doctype_
        , meta_ [charset_ "utf-8"]
        , mconcat (((\x -> link_ [x,rel_ "stylesheet"]) . href_) <$> (p'^.pageLibsCss))
        , cssInline
        , mconcat ((\x -> with (script_ mempty) [src_ x]) <$> p'^.pageLibsJs)
        , p'^.pageHtmlHeader
        , p'^.pageHtmlBody
        , jsInline
        ]  
      Svg ->
        Html.doctypesvg_ <> 
        svg_ 
        (Html.defs_ $ mconcat
         [ renderLibsJsSvg (fromStrict <$> p'^.pageLibsJs)
         , renderLibsCssSvg (fromStrict <$> p'^.pageLibsCss)
         , cssInline
         , jsInline
         , p'^.pageHtmlBody
         ])
    css = rendercss (p'^.pageCss)
    js = renderjs (p'^.pageJsGlobal <> Js.onLoad (p'^.pageJsOnLoad))
    (renderjs, rendercss) = renderers $ pc^.pagecRender 
    (cssInline, jsInline) = 
      case pc^.pagecConcerns of
        Separated    -> (mempty,mempty)
        Inline -> (style_ [type_ (toStrict $ pack "text/css")] css, script_ mempty js)

renderPageToFile :: PageConfig -> Page -> IO ()
renderPageToFile pc@(PageConfig _ _ _ _ files) page =
  liftIO $ liftA2 writeFile' files (renderPageText pc page)
  where
    writeFile' fp s = unless (fp==mempty) (writeFile fp s)

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
