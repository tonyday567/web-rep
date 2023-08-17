{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Web.Rep.Page
  ( -- $page
    Page (..),
    PageConfig (..),
    defaultPageConfig,
    Concerns (..),
    suffixes,
    concernNames,
    PageConcerns (..),
    PageStructure (..),
    PageRender (..),
    -- $css
    Css (..),
    renderCss,
    -- $js
    Js (..),
    onLoad,
    renderJs,
  )
where

import Data.String.Interpolate
import Data.Text (Text, unpack)
import Data.Text qualified as Text
import GHC.Generics
import Lucid
import Optics.Core

-- | Components of a web page.
--
-- A web page can take many forms but still have the same underlying representation. For example, CSS can be linked to in a separate file, or can be inline within html, but still be the same css and have the same expected external effect. A Page represents the practical components of what makes up a static snapshot of a web page.
data Page = Page
  { -- | css library links
    libsCss :: [Html ()],
    -- | javascript library links
    libsJs :: [Html ()],
    -- | css
    cssBody :: Css,
    -- | javascript with global scope
    jsGlobal :: Js,
    -- | javascript included within the onLoad function
    jsOnLoad :: Js,
    -- | html within the header
    htmlHeader :: Html (),
    -- | body html
    htmlBody :: Html ()
  }
  deriving (Show, Generic)

instance Semigroup Page where
  (<>) p0 p1 =
    Page
      (p0 ^. #libsCss <> p1 ^. #libsCss)
      (p0 ^. #libsJs <> p1 ^. #libsJs)
      (p0 ^. #cssBody <> p1 ^. #cssBody)
      (p0 ^. #jsGlobal <> p1 ^. #jsGlobal)
      (p0 ^. #jsOnLoad <> p1 ^. #jsOnLoad)
      (p0 ^. #htmlHeader <> p1 ^. #htmlHeader)
      (p0 ^. #htmlBody <> p1 ^. #htmlBody)

instance Monoid Page where
  mempty = Page [] [] mempty mempty mempty mempty mempty

  mappend = (<>)

-- | A web page typically is composed of some css, javascript and html.
--
-- 'Concerns' abstracts this structural feature of a web page.
data Concerns a = Concerns
  { cssConcern :: a,
    jsConcern :: a,
    htmlConcern :: a
  }
  deriving (Eq, Show, Foldable, Traversable, Generic)

instance Functor Concerns where
  fmap f (Concerns c j h) = Concerns (f c) (f j) (f h)

instance Applicative Concerns where
  pure a = Concerns a a a

  Concerns f g h <*> Concerns a b c = Concerns (f a) (g b) (h c)

-- | The common file suffixes of the three concerns.
suffixes :: Concerns FilePath
suffixes = Concerns ".css" ".js" ".html"

-- | Create filenames for each Concern element.
concernNames :: FilePath -> FilePath -> Concerns FilePath
concernNames dir stem =
  (\x -> dir <> stem <> x) <$> suffixes

-- | Is the rendering to include all 'Concerns' (typically in a html file) or be separated (tyypically into separate files and linked in the html file)?
data PageConcerns
  = Inline
  | Separated
  deriving (Show, Eq, Generic)

-- | Various ways that a Html file can be structured.
data PageStructure
  = HeaderBody
  | Headless
  | Snippet
  | Svg
  deriving (Show, Eq, Generic)

-- | Post-processing of page concerns
data PageRender
  = Pretty
  | Minified
  | NoPost
  deriving (Show, Eq, Generic)

-- | Configuration options when rendering a 'Page'.
data PageConfig = PageConfig
  { concerns :: PageConcerns,
    structure :: PageStructure,
    pageRender :: PageRender,
    filenames :: Concerns FilePath,
    localdirs :: [FilePath]
  }
  deriving (Show, Eq, Generic)

-- | Default configuration is inline ecma and css, separate html header and body, minified code, with the suggested filename prefix.
defaultPageConfig :: FilePath -> PageConfig
defaultPageConfig stem =
  PageConfig
    Inline
    HeaderBody
    Minified
    ((stem <>) <$> suffixes)
    []

-- | css as Text.
newtype Css = Css {cssText :: Text} deriving (Generic, Semigroup, Monoid)

instance Show Css where
  show (Css txt) = unpack txt

-- | Render 'Css' as text.
renderCss :: PageRender -> Css -> Text
renderCss Minified = Text.filter (\c -> c /= ' ' && c /= '\n') . cssText
renderCss _ = cssText

-- | Javascript as Text
newtype Js = Js {jsText :: Text} deriving (Eq, Show, Generic, Semigroup, Monoid)

onLoad :: Js -> Js
onLoad (Js t) = Js [i| window.onload=function(){#{t}};|]

-- | Render 'Js' as 'Text'.
renderJs :: Js -> Text
renderJs = jsText
