{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Page.Types
  ( Page(Page)
  , PageText(PageText)
  , PageConfig(PageConfig)
  , defaultPageConfig
  , Concern(..)
  , Concerns(Concerns)
  , suffixes
  , concernNames
  , PageConcerns(..)
  , PageStructure(..)
  , PageRender(..)
  ) where

import Control.Lens
import Data.Generics.Labels()
import Data.Semigroup ((<>))
import Lucid
import Protolude hiding ((<>))
import qualified Web.Page.Css as Css
import qualified Web.Page.Js as Js

data Page =
    Page
    { libsCss :: [Html ()]
    , libsJs :: [Html ()]
    , cssBody :: Css.PageCss
    , jsGlobal :: Js.PageJs
    , jsOnLoad :: Js.PageJs
    , htmlHeader :: Html ()
    , htmlBody :: Html ()
    } deriving (Show, Generic)

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

data PageText =
    PageText
    { libsCssText :: [Text]
    , libsJsText :: [Text]
    , cssBodyText :: Text
    , jsGlobalText :: Text
    , jsOnLoadText :: Text
    , htmlHeaderText :: Text
    , htmlBodyText :: Text
    } deriving (Show, Generic)

data Concern = Css | Js | Html deriving (Show, Eq, Generic)

data Concerns a =
  Concerns
  { css :: a
  , js :: a
  , html :: a
  } deriving (Eq, Show, Foldable, Traversable, Generic)

instance Functor Concerns where
  fmap f (Concerns c j h) = Concerns (f c) (f j) (f h)

instance Applicative Concerns where
  pure a = Concerns a a a
  Concerns f g h <*> Concerns a b c = Concerns (f a) (g b) (h c)

suffixes :: Concerns FilePath
suffixes = Concerns ".css" ".js" ".html"

concernNames :: FilePath -> FilePath -> Concerns FilePath
concernNames dir stem =
  (\x->dir<>stem<>x) <$> suffixes

data PageConcerns =
  Inline |
  Separated
  deriving (Show, Eq, Generic)

data PageStructure =
  HeaderBody |
  Headless |
  Snippet |
  Svg
  deriving (Show, Eq, Generic)

data PageRender =
  Pretty |
  Minified
  deriving (Show, Eq, Generic)

data PageConfig =
  PageConfig
  { concerns :: PageConcerns
  , structure :: PageStructure
  , pageRender :: PageRender
  , filenames :: Concerns FilePath
  , localdirs :: [FilePath]
  } deriving (Show, Eq, Generic)

defaultPageConfig :: PageConfig
defaultPageConfig = PageConfig Inline HeaderBody Minified
    (("default"<>) <$> suffixes) []
