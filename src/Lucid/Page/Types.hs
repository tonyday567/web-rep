{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lucid.Page.Types where

import Control.Applicative
import Control.Lens
import Data.Default
import Data.Foldable
import Data.Text
import Data.Monoid
import Lucid
import Lucid.Page.Css
import Lucid.Page.Js

data Page =
    Page
    { _pageLibsCss :: [Text]
    , _pageLibsJs :: [Text]
    , _pageCss :: Css
    , _pageJsGlobal :: JStat
    , _pageJsOnLoad :: JStat
    , _pageHtmlHeader :: Html ()
    , _pageHtmlBody :: Html ()
    } deriving Show

makeLenses ''Page

instance Monoid Page where
    mempty = Page [] [] mempty mempty mempty mempty mempty
    mappend p0 p1 =
        Page
        (p0^.pageLibsCss <> p1^.pageLibsCss)
        (p0^.pageLibsJs <> p1^.pageLibsJs)
        (p0^.pageCss        <> p1^.pageCss)
        (p0^.pageJsGlobal   <> p1^.pageJsGlobal)
        (p0^.pageJsOnLoad   <> p1^.pageJsOnLoad)
        (p0^.pageHtmlHeader <> p1^.pageHtmlHeader)
        (p0^.pageHtmlBody   <> p1^.pageHtmlBody)

data Concern = Css | Js | Html deriving Show
data Concerns a = Concerns { _css::a, _js::a, _html::a } deriving (Eq, Show, Foldable, Traversable)

instance Functor Concerns where
  fmap f (Concerns c j h) = Concerns (f c) (f j) (f h)

instance Applicative Concerns where
  pure a = Concerns a a a
  Concerns f g h <*> Concerns a b c = Concerns (f a) (g b) (h c)

concernNames :: FilePath -> FilePath -> Concerns FilePath
concernNames dir stem = (\x->dir<>stem<>x) <$> Concerns ".css" ".js" ".html"

data PageConcerns = Inline | Separated deriving (Show, Eq)
data PageStructure = HeaderBody | Headless | Svg deriving (Show, Eq)
data PageLibs = LocalLibs FilePath | LinkedLibs deriving (Show, Eq)
data PageRender = Pretty | Minified deriving (Show, Eq)

data PageConfig =
  PageConfig
  { _pagecConcerns :: PageConcerns
  , _pagecStructure :: PageStructure
  , _pagecRender :: PageRender
  , _pagecLibs :: PageLibs
  , _pagecFilenames :: Concerns FilePath
  } deriving (Show, Eq)

instance Default PageConfig where
    def = PageConfig Inline HeaderBody Minified LinkedLibs (Concerns "def.css" "def.js" "def.html")

makeLenses ''PageConfig

