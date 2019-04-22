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
import Data.Text.IO (writeFile)
import Data.Text.Lazy (fromStrict, toStrict)
import Data.Traversable
import Lucid
import Prelude hiding (writeFile)
import Web.Page.Html
import Web.Page.Types
import qualified Data.Text as Text
import qualified Lucid.Svg as Svg
import qualified Web.Page.Css as Css
import qualified Web.Page.Js as Js

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
            , mconcat libsJs'
            , jsInline
            ]))
        Headless ->
          mconcat
            [ doctype_
            , meta_ [charset_ "utf-8"]
            , mconcat libsCss'
            , cssInline
            , p ^. #htmlHeader
            , p ^. #htmlBody
            , mconcat libsJs'
            , jsInline
            ]
        Snippet ->
          mconcat
            [ mconcat libsCss'
            , cssInline
            , p ^. #htmlHeader
            , p ^. #htmlBody
            , mconcat libsJs'
            , jsInline
            ]
        Svg ->
          Svg.doctype_ <>
          svg_
            (Svg.defs_ $
             mconcat
               [ mconcat libsCss'
               , cssInline
               , p ^. #htmlBody
               , mconcat libsJs'
               , jsInline
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
            , mconcat (toHtmlRaw <$> libsJs')
            , toHtmlRaw jsInline
            ]))
        Headless ->
          mconcat
            [ doctype_
            , meta_ [charset_ "utf-8"]
            , mconcat (toHtmlRaw <$> libsCss')
            , toHtmlRaw cssInline
            , toHtmlRaw $ p ^. #htmlHeaderText
            , toHtmlRaw $ p ^. #htmlBodyText
            , mconcat (toHtmlRaw <$> libsJs')
            , toHtmlRaw jsInline
            ]
        Snippet ->
          mconcat
            [ mconcat (toHtmlRaw <$> libsCss')
            , toHtmlRaw cssInline
            , toHtmlRaw $ p ^. #htmlHeaderText
            , toHtmlRaw $ p ^. #htmlBodyText
            , mconcat (toHtmlRaw <$> libsJs')
            , toHtmlRaw jsInline
            ]
        Svg ->
          Svg.doctype_ <>
          svg_
            (Svg.defs_ $
             mconcat
               [ mconcat (toHtmlRaw <$> libsCss')
               , toHtmlRaw cssInline
               , toHtmlRaw $ p ^. #htmlBodyText
               , mconcat (toHtmlRaw <$> libsJs')
               , toHtmlRaw jsInline
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

