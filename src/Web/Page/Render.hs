{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Web.Page.Render
  ( renderPage
  , renderPageWith
  , renderPageHtmlWith
  , renderPageAsText
  , renderPageToFile
  , renderPageHtmlToFile
  , renderPageTextWith
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Text.IO (writeFile)
import Data.Traversable
import Lucid
import qualified Lucid.Svg as Svg
import Prelude hiding (writeFile)
import qualified Web.Page.Css as Css
import qualified Web.Page.Js as Js
import Web.Page.Types
import Web.Page.Html

renderPage :: Page -> Html ()
renderPage p =
  (\(_, _, x) -> x) $ renderPageWith defaultPageConfig p

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
          doctype_ <>
          with html_ [lang_ "en"]
          (head_
          (mconcat
            [ meta_ [charset_ "utf-8"]
            , cssInline
            , mconcat libsCss'
            , p ^. #htmlHeader
            ]) <>
          body_
          (mconcat
            [ p ^. #htmlBody
            , jsInline
            , mconcat libsJs'
            ]))
        Headless ->
          mconcat
            [ doctype_
            , meta_ [charset_ "utf-8"]
            , mconcat libsCss'
            , cssInline
            , mconcat libsJs'
            , p ^. #htmlHeader
            , p ^. #htmlBody
            , jsInline
            ]
        Snippet ->
          mconcat
            [ mconcat libsCss'
            , cssInline
            , mconcat libsJs'
            , p ^. #htmlHeader
            , p ^. #htmlBody
            , jsInline
            ]
        Svg ->
          Svg.doctype_ <>
          svg_
            (Svg.defs_ $
             mconcat
               [ mconcat libsJs'
               , mconcat libsCss'
               , cssInline
               , jsInline
               , p ^. #htmlBody
               ])
    css = rendercss (p ^. #cssBody)
    js = renderjs (p ^. #jsGlobal <> Js.onLoad (p ^. #jsOnLoad))
    (renderjs, rendercss) = renderers $ pc ^. #pageRender
    cssInline
      | pc ^. #concerns == Separated || css == mempty = mempty
      | otherwise = style_ [type_ "text/css"] css
    jsInline
      | pc ^. #concerns == Separated || js == mempty = mempty
      | otherwise = script_ mempty js
    libsCss' =
      case pc ^. #concerns of
        Inline -> p ^. #libsCss
        Separated ->
          p ^. #libsCss <>
          [libCss (Text.pack $ pc ^. #filenames . #css)]
    libsJs' =
      case pc ^. #concerns of
        Inline -> p ^. #libsJs
        Separated ->
          p ^. #libsJs <>
          [libJs (Text.pack $ pc ^. #filenames . #js)]

renderPageToFile :: FilePath -> PageConfig -> Page -> IO ()
renderPageToFile dir pc page =
  void $ sequenceA $ liftA2 writeFile' (pc ^. #filenames) (renderPageAsText pc page)
  where
    writeFile' fp s = unless (s == mempty) (writeFile (dir <> "/" <> fp) s)

renderPageHtmlToFile :: FilePath -> PageConfig -> Page -> IO ()
renderPageHtmlToFile file pc page =
  writeFile file (toText $ renderPageHtmlWith pc page)

renderPageAsText :: PageConfig -> Page -> Concerns Text
renderPageAsText pc p =
  case pc ^. #concerns of
    Inline -> Concerns mempty mempty htmlt
    Separated -> Concerns css js htmlt
  where
    htmlt = toText h
    (css, js, h) = renderPageWith pc p

rendererJs :: PageRender -> Js.PageJs -> Text
rendererJs _ (Js.PageJsText js) = js
rendererJs Minified (Js.PageJs js) = toStrict . Js.renderToText . Js.minifyJS . Js.unJS $ js
rendererJs Pretty (Js.PageJs js) = toStrict . Js.renderToText . Js.unJS $ js

rendererCss :: PageRender -> Css.PageCss -> Text
rendererCss Minified (Css.PageCss css) = toStrict $ Css.renderWith Css.compact [] css
rendererCss Pretty (Css.PageCss css) = toStrict $ Css.render css
rendererCss _ (Css.PageCssText css) = css

renderers :: PageRender -> (Js.PageJs -> Text, Css.PageCss -> Text)
renderers p = (rendererJs p, rendererCss p)

renderPageTextWith :: PageConfig -> PageText -> (Text, Text, Html ())
renderPageTextWith pc p =
  case pc ^. #concerns of
    Inline -> (mempty, mempty, h)
    Separated -> (css, js, h)
  where
    h =
      case pc ^. #structure of
        HeaderBody ->
          doctype_ <>
          with html_ [lang_ "en"]
          (head_
          (mconcat
            [ meta_ [charset_ "utf-8"]
            , toHtmlRaw cssInline
            , mconcat (toHtmlRaw <$> libsCss')
            , toHtmlRaw $ p ^. #htmlHeaderText
            ]) <>
          body_
          (mconcat
            [ toHtmlRaw $ p ^. #htmlBodyText
            , toHtmlRaw jsInline
            , mconcat (toHtmlRaw <$> libsJs')
            ]))
        Headless ->
          mconcat
            [ doctype_
            , meta_ [charset_ "utf-8"]
            , mconcat (toHtmlRaw <$> libsCss')
            , toHtmlRaw cssInline
            , mconcat (toHtmlRaw <$> libsJs')
            , toHtmlRaw $ p ^. #htmlHeaderText
            , toHtmlRaw $ p ^. #htmlBodyText
            , toHtmlRaw jsInline
            ]
        Snippet ->
          mconcat
            [ mconcat (toHtmlRaw <$> libsCss')
            , toHtmlRaw cssInline
            , mconcat (toHtmlRaw <$> libsJs')
            , toHtmlRaw $ p ^. #htmlHeaderText
            , toHtmlRaw $ p ^. #htmlBodyText
            , toHtmlRaw jsInline
            ]
        Svg ->
          Svg.doctype_ <>
          svg_
            (Svg.defs_ $
             mconcat
               [ mconcat (toHtmlRaw <$> libsJs')
               , mconcat (toHtmlRaw <$> libsCss')
               , toHtmlRaw cssInline
               , toHtmlRaw jsInline
               , toHtmlRaw $ p ^. #htmlBodyText
               ])
    css = p ^. #cssBodyText
    js = rendererJs Pretty (Js.PageJsText $ p ^. #jsGlobalText) <>
         rendererJs Pretty (Js.onLoad (Js.PageJsText $ p ^. #jsOnLoadText))
    cssInline
      | pc ^. #concerns == Separated || css == mempty = mempty
      | otherwise = renderText $ style_ [type_ "text/css"] css
    jsInline
      | pc ^. #concerns == Separated || js == mempty = mempty
      | otherwise = renderText $ script_ mempty js
    libsCss' =
      case pc ^. #concerns of
        Inline -> fromStrict <$> p ^. #libsCssText
        Separated ->
          (fromStrict <$> p ^. #libsCssText) <>
          [renderText $ libCss (Text.pack $ pc ^. #filenames . #css)]
    libsJs' =
      case pc ^. #concerns of
        Inline -> fromStrict <$> p ^. #libsJsText
        Separated ->
          (fromStrict <$> p ^. #libsJsText) <>
          [renderText $ libJs (Text.pack $ pc ^. #filenames . #js)]

