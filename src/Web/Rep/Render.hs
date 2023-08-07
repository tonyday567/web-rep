{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Page rendering
module Web.Rep.Render
  ( renderPage,
    renderPageWith,
    renderPageHtmlWith,
    renderPageAsText,
    renderPageToFile,
    renderPageHtmlToFile,
  )
where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Text (Text, pack, unpack)
import Lucid
import Optics.Core
import Web.Rep.Html
import Web.Rep.Page

-- | Render a Page with the default configuration into Html.
renderPage :: Page -> Html ()
renderPage p =
  (\(_, _, x) -> x) $ renderPageWith (defaultPageConfig "default") p

-- | Render a Page into Html.
renderPageHtmlWith :: PageConfig -> Page -> Html ()
renderPageHtmlWith pc p =
  (\(_, _, x) -> x) $ renderPageWith pc p

-- | Render a Page into css text, js text and html.
renderPageWith :: PageConfig -> Page -> (Text, Text, Html ())
renderPageWith pc p =
  case pc ^. #concerns of
    Inline -> (mempty, mempty, h)
    Separated -> (css, js, h)
  where
    h =
      case pc ^. #structure of
        HeaderBody ->
          doctype_
            <> with
              html_
              [lang_ "en"]
              ( head_
                  ( mconcat
                      [ meta_ [charset_ "utf-8"],
                        cssInline,
                        mconcat libsCss',
                        p ^. #htmlHeader
                      ]
                  )
                  <> body_
                    ( mconcat
                        [ p ^. #htmlBody,
                          mconcat libsJs',
                          jsInline
                        ]
                    )
              )
        Headless ->
          mconcat
            [ doctype_,
              meta_ [charset_ "utf-8"],
              mconcat libsCss',
              cssInline,
              p ^. #htmlHeader,
              p ^. #htmlBody,
              mconcat libsJs',
              jsInline
            ]
        Snippet ->
          mconcat
            [ mconcat libsCss',
              cssInline,
              p ^. #htmlHeader,
              p ^. #htmlBody,
              mconcat libsJs',
              jsInline
            ]
        Svg ->
          svgDocType
            <> svg_
              ( svgDefs $
                  mconcat
                    [ mconcat libsCss',
                      cssInline,
                      p ^. #htmlBody,
                      mconcat libsJs',
                      jsInline
                    ]
              )
    css = rendercss (p ^. #cssBody)
    js = renderjs (p ^. #jsGlobal <> onLoad (p ^. #jsOnLoad))
    renderjs = renderRepJs $ pc ^. #pageRender
    rendercss = renderCss $ pc ^. #pageRender
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
          p ^. #libsCss
            <> [libCss (pack $ pc ^. #filenames % #cssConcern)]
    libsJs' =
      case pc ^. #concerns of
        Inline -> p ^. #libsJs
        Separated ->
          p ^. #libsJs
            <> [libJs (pack $ pc ^. #filenames % #jsConcern)]

-- | Render Page concerns to files.
renderPageToFile :: FilePath -> PageConfig -> Page -> IO ()
renderPageToFile dir pc page =
  sequenceA_ $ liftA2 writeFile' (pc ^. #filenames) (fmap unpack (renderPageAsText pc page))
  where
    writeFile' fp s = unless (s == mempty) (writeFile (dir <> "/" <> fp) s)

-- | Render a page to just a Html file.
renderPageHtmlToFile :: FilePath -> PageConfig -> Page -> IO ()
renderPageHtmlToFile file pc page =
  writeFile file (unpack $ toText $ renderPageHtmlWith pc page)

-- | Render a Page as Text.
renderPageAsText :: PageConfig -> Page -> Concerns Text
renderPageAsText pc p =
  case pc ^. #concerns of
    Inline -> Concerns mempty mempty htmlt
    Separated -> Concerns css js htmlt
  where
    htmlt = toText h
    (css, js, h) = renderPageWith pc p

svgDocType :: Html ()
svgDocType = "?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\""

svgDefs :: Html () -> Html ()
svgDefs = term "defs"
