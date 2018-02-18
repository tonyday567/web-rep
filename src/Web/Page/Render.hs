{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Web.Page.Render
  ( renderPage
  , renderPageWith
  , renderPageHtmlWith
  , renderPageText
  , renderPageToFile
  , renderPageHtmlToFile
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Default
import Data.Monoid
import qualified Data.Text as Text
import Data.Text.Lazy (Text, fromStrict, pack, toStrict)
import Data.Text.Lazy.IO (writeFile)
import Data.Traversable
import Lucid
import Prelude hiding (writeFile)
import qualified Web.Page.Css as Css
import qualified Web.Page.Html as Html
import qualified Web.Page.Js as Js
import Web.Page.Types

-- import qualified Text.Blaze.Html5 as Html5
renderPage :: Page -> Html ()
renderPage p =
  (\(_, _, x) -> x) $ renderPageWith def p

renderPageHtmlWith :: PageConfig -> Page -> Html ()
renderPageHtmlWith pc p =
  (\(_, _, x) -> x) $ renderPageWith pc p

renderPageWith :: PageConfig -> Page -> (Text, Text, Html ())
renderPageWith pc p =
  case pc ^. #concerns of
    Inline -> (mempty, mempty, h)
    Separated -> (css, js, h)
  where
    h =
      case pc ^. #structure of
        HeaderBody ->
          html_ $
          head_ $
          mconcat
            [ meta_ [charset_ "utf-8"]
            , mconcat
                (((\x -> link_ [x, rel_ "stylesheet"]) . href_) <$> libsCss')
            , mconcat ((\x -> with (script_ mempty) [src_ x]) <$> libsJs')
            , jsInline
            , cssInline
            , p ^. #htmlHeader
            ] <>
          body_ (p ^. #htmlBody)
        Headless ->
          mconcat
            [ doctype_
            , meta_ [charset_ "utf-8"]
            , mconcat
                (((\x -> link_ [x, rel_ "stylesheet"]) . href_) <$> libsCss')
            , cssInline
            , mconcat ((\x -> with (script_ mempty) [src_ x]) <$> libsJs')
            , p ^. #htmlHeader
            , p ^. #htmlBody
            , jsInline
            ]
        Svg ->
          Html.doctypesvg_ <>
          svg_
            (Html.defs_ $
             mconcat
               [ renderLibsJsSvg (fromStrict <$> libsJs')
               , renderLibsCssSvg (fromStrict <$> libsCss')
               , cssInline
               , jsInline
               , p ^. #htmlBody
               ])
    css = rendercss (p ^. #cssBody)
    js = renderjs (p ^. #jsGlobal <> Js.onLoad (p ^. #jsOnLoad))
    (renderjs, rendercss) = renderers $ pc ^. #pageRender
    cssInline
      | pc ^. #concerns == Separated || css == mempty = mempty
      | otherwise = style_ [type_ (toStrict $ pack "text/css")] css
    jsInline
      | pc ^. #concerns == Separated || js == mempty = mempty
      | otherwise = script_ mempty js
    libsCss =
      case pc ^. #pageLibs of
        LinkedLibs -> p ^. #libsCss
        LocalLibs dir -> (\x -> Text.pack dir <> x) <$> p ^. #libsCss
    libsCss' =
      case pc ^. #concerns of
        Inline -> libsCss
        Separated -> libsCss <> [Text.pack (pc ^. #filenames . #css)]
    libsJs =
      case pc ^. #pageLibs of
        LinkedLibs -> p ^. #libsJs
        LocalLibs dir -> (\x -> Text.pack dir <> x) <$> p ^. #libsJs
    libsJs' =
      case pc ^. #concerns of
        Inline -> libsJs
        Separated -> libsJs <> [Text.pack (pc ^. #filenames. #js)]

renderPageToFile :: FilePath -> PageConfig -> Page -> IO ()
renderPageToFile dir pc@(PageConfig _ _ _ _ files) page =
  void $ sequenceA $ liftA2 writeFile' files (renderPageText pc page)
  where
    writeFile' fp s = unless (s == mempty) (writeFile (dir <> "/" <> fp) s)

renderPageHtmlToFile :: FilePath -> PageConfig -> Page -> IO ()
renderPageHtmlToFile file pc page =
  writeFile file (renderText $ renderPageHtmlWith pc page)

renderPageText :: PageConfig -> Page -> Concerns Text
renderPageText pc p =
  case pc ^. #concerns of
    Inline -> Concerns mempty mempty htmlt
    Separated -> Concerns css js htmlt
  where
    htmlt = renderText h
    (css, js, h) = renderPageWith pc p

renderLibsJsSvg :: [Text] -> Html ()
renderLibsJsSvg xs = mconcat $ libJsSvg <$> xs

libJsSvg :: Text -> Html ()
libJsSvg x =
  with
    (script_ mempty)
    [ href_ (toStrict x)
    , rel_ "stylesheet"
    , type_ (toStrict $ pack "text/css")
    , Html.makeAttribute "xmlns" "http://www.w3.org/1999/xhtml"
    ]

renderLibsCssSvg :: [Text] -> Html ()
renderLibsCssSvg xs =
  mconcat
    ((with Html.linksvg_ .
      (\x ->
         [ href_ (toStrict x)
         , rel_ "stylesheet"
         , type_ (toStrict $ pack "text/css")
         , Html.makeAttribute "xmlns" "http://www.w3.org/1999/xhtml"
         ])) <$>
     xs)
    mempty

renderers :: PageRender -> (Js.JS -> Text, Css.Css -> Text)
renderers Minified = (Js.renderToText . Js.minifyJS . Js.unJS, Css.renderWith Css.compact [])
renderers Pretty = (Js.renderToText . Js.unJS, Css.render)
