{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Page rendering
module Web.Rep.Render
  ( renderPage,
    renderPageWith,
    renderPageHtmlWith,
    renderPageAsByteString,
    renderPageToFile,
    renderPageHtmlToFile,
  )
where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Optics.Core
import Web.Rep.Html
import Web.Rep.Page
import MarkupParse
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import FlatParse.Basic (strToUtf8)
import Data.Tree

-- | Render a Page with the default configuration into Html.
renderPage :: Page -> Markup
renderPage p =
  (\(_, _, x) -> x) $ renderPageWith (defaultPageConfig "default") p

-- | Render a Page into Html.
renderPageHtmlWith :: PageConfig -> Page -> Markup
renderPageHtmlWith pc p =
  (\(_, _, x) -> x) $ renderPageWith pc p

-- | Render a Page into css text, js text and html.
renderPageWith :: PageConfig -> Page -> (ByteString, ByteString, Markup)
renderPageWith pc p =
  case pc ^. #concerns of
    Inline -> (mempty, mempty, h)
    Separated -> (css, js, h)
  where
    h =
      case pc ^. #structure of
        HeaderBody ->
          Markup Html (doctypeHtml <>
           [wrap "html"
              [Attr "lang" "en"]
              [wrap "head" []
                      ([ pure ( tag "meta" [Attr "charset" "utf-8"] )] <>
                        cssInline <>
                        libsCss' <>
                        view #htmlHeader p),
               wrap "body" []
                    (
                        view #htmlBody p <>
                        libsJs' <>
                        jsInline
                    )
              ]
          ])
        Headless ->
            Markup Html $ doctypeHtml <>
              [pure $ tag "meta" [Attr "charset" "utf-8"]] <>
              libsCss' <>
              cssInline <>
              ( view #htmlHeader p  )<>
              p ^. #htmlBody <>
              libsJs' <>
              jsInline
        Snippet ->
            Markup Html $
              libsCss' <>
              cssInline <>
              view #htmlHeader p <>
              view #htmlBody p <>
              libsJs' <>
              jsInline

    css :: ByteString
    css = renderCss (pc ^. #pageRender) (p ^. #cssBody)

    js :: ByteString
    js = jsByteString (p ^. #jsGlobal <> onLoad (p ^. #jsOnLoad))
    cssInline
      | pc ^. #concerns == Separated || css == mempty = mempty
      | otherwise = pure $ wrap "style" [Attr "type" "text/css"] (pure $ pure $ Content css)
    jsInline
      | pc ^. #concerns == Separated || js == mempty = mempty
      | otherwise = pure $ wrap "script" [] (pure $ pure $ Content js)
    libsCss' :: [Tree Token]
    libsCss' =
      case pc ^. #concerns of
        Inline -> view #libsCss p
        Separated ->
          view #libsCss p
            <> [libCss (strToUtf8 $ pc ^. #filenames % #cssConcern)]
    libsJs' :: [Tree Token]
    libsJs' =
      case pc ^. #concerns of
        Inline -> p ^. #libsJs
        Separated ->
          p ^. #libsJs
            <> [libJs (strToUtf8 $ pc ^. #filenames % #jsConcern)]

-- | Render Page concerns to files.
renderPageToFile :: FilePath -> PageConfig -> Page -> IO ()
renderPageToFile dir pc page =
  sequenceA_ $ liftA2 writeFile' (pc ^. #filenames) (renderPageAsByteString pc page)
  where
    writeFile' fp s = unless (s == mempty) (B.writeFile (dir <> "/" <> fp) s)

-- | Render a page to just a Html file.
renderPageHtmlToFile :: FilePath -> PageConfig -> Page -> IO ()
renderPageHtmlToFile file pc page =
  B.writeFile file (markdown Compact $ renderPageHtmlWith pc page)

-- | Render a Page as Text.
renderPageAsByteString :: PageConfig -> Page -> Concerns ByteString
renderPageAsByteString pc p =
  case pc ^. #concerns of
    Inline -> Concerns mempty mempty (markdown Compact h)
    Separated -> Concerns css js (markdown Compact h)
  where
    (css, js, h) = renderPageWith pc p
