{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Web.Page.Types
  ( Page(Page)
  , PageConfig(PageConfig)
  , Concern(..)
  , Concerns(Concerns)
  , suffixes
  , concernNames
  , PageConcerns(..)
  , PageStructure(..)
  , PageRender(..)
  , PageLibs(..)
  ) where

import Control.Lens
import Data.Default
import Data.Generics.Labels()
import Lucid
import Protolude
import qualified Web.Page.Css as Css
import qualified Web.Page.Js as Js

data Page =
    Page
    { libsCss :: [Text]
    , libsJs :: [Text]
    , cssBody :: Css.Css
    , jsGlobal :: Js.JS
    , jsOnLoad :: [Js.JSStatement]
    , htmlHeader :: Html ()
    , htmlBody :: Html ()
    } deriving (Show, Generic)

makeLenses ''Page

instance Monoid Page where
  mempty = Page [] [] mempty mempty mempty mempty mempty
  mappend p0 p1 =
    Page
    (p0 ^. #libsCss <> p1 ^. #libsCss)
    (p0 ^. #libsJs <> p1 ^. #libsJs)
    (p0 ^. #cssBody <> p1 ^. #cssBody)
    (p0 ^. #jsGlobal <> p1 ^. #jsGlobal)
    (p0 ^. #jsOnLoad <> p1 ^. #jsOnLoad)
    (p0 ^. #htmlHeader <> p1 ^. #htmlHeader)
    (p0 ^. #htmlBody <> p1 ^. #htmlBody)

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
  Svg
  deriving (Show, Eq, Generic)

data PageLibs =
  LocalLibs FilePath |
  LinkedLibs
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
  , pageLibs :: PageLibs
  , filenames :: Concerns FilePath
  } deriving (Show, Eq, Generic)

instance Default PageConfig where
  def = PageConfig Inline HeaderBody Minified LinkedLibs
    (Concerns "def.css" "def.js" "def.html")
