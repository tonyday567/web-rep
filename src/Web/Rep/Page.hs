{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_HADDOCK hide, not-home #-}

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
    Css,
    RepCss (..),
    renderCss,
    renderRepCss,
    -- $js
    JS (..),
    RepJs (..),
    onLoad,
    renderRepJs,
    parseJs,
    renderJs,
  )
where

import Clay (Css)
import qualified Clay
import Data.Text (Text, unpack)
import Data.Text.Lazy (toStrict)
import GHC.Generics
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Language.JavaScript.Process.Minify
import Lucid
import Optics.Core
import Text.InterpolatedString.Perl6

-- | Components of a web page.
--
-- A web page can take many forms but still have the same underlying representation. For example, CSS can be linked to in a separate file, or can be inline within html, but still be the same css and have the same expected external effect. A Page represents the practical components of what makes up a static snapshot of a web page.
data Page = Page
  { -- | css library links
    libsCss :: [Html ()],
    -- | javascript library links
    libsJs :: [Html ()],
    -- | css
    cssBody :: RepCss,
    -- | javascript with global scope
    jsGlobal :: RepJs,
    -- | javascript included within the onLoad function
    jsOnLoad :: RepJs,
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

-- | Unifies css as either a 'Clay.Css' or as Text.
data RepCss = RepCss Clay.Css | RepCssText Text deriving (Generic)

instance Show RepCss where
  show (RepCss css) = unpack . renderCss $ css
  show (RepCssText txt) = unpack txt

instance Semigroup RepCss where
  (<>) (RepCss css) (RepCss css') = RepCss (css <> css')
  (<>) (RepCssText css) (RepCssText css') = RepCssText (css <> css')
  (<>) (RepCss css) (RepCssText css') =
    RepCssText (renderCss css <> css')
  (<>) (RepCssText css) (RepCss css') =
    RepCssText (css <> renderCss css')

instance Monoid RepCss where
  mempty = RepCssText mempty

  mappend = (<>)

-- | Render 'RepCss' as text.
renderRepCss :: PageRender -> RepCss -> Text
renderRepCss Minified (RepCss css) = toStrict $ Clay.renderWith Clay.compact [] css
renderRepCss _ (RepCss css) = toStrict $ Clay.render css
renderRepCss _ (RepCssText css) = css

-- | Render 'Css' as text.
renderCss :: Css -> Text
renderCss = toStrict . Clay.render

-- | wrapper for `JSAST`
newtype JS = JS {unJS :: JSAST} deriving (Show, Eq, Generic)

instance Semigroup JS where
  (<>) (JS (JSAstProgram ss ann)) (JS (JSAstProgram ss' _)) =
    JS $ JSAstProgram (ss <> ss') ann
  (<>) (JS (JSAstProgram ss ann)) (JS (JSAstStatement s _)) =
    JS $ JSAstProgram (ss <> [s]) ann
  (<>) (JS (JSAstProgram ss ann)) (JS (JSAstExpression e ann')) =
    JS $ JSAstProgram (ss <> [JSExpressionStatement e (JSSemi ann')]) ann
  (<>) (JS (JSAstProgram ss ann)) (JS (JSAstLiteral e ann')) =
    JS $ JSAstProgram (ss <> [JSExpressionStatement e (JSSemi ann')]) ann
  (<>) (JS (JSAstStatement s ann)) (JS (JSAstProgram ss _)) =
    JS $ JSAstProgram (s : ss) ann
  (<>) (JS (JSAstStatement s ann)) (JS (JSAstStatement s' _)) =
    JS $ JSAstProgram [s, s'] ann
  (<>) (JS (JSAstStatement s ann)) (JS (JSAstExpression e ann')) =
    JS $ JSAstProgram [s, JSExpressionStatement e (JSSemi ann')] ann
  (<>) (JS (JSAstStatement s ann)) (JS (JSAstLiteral e ann')) =
    JS $ JSAstProgram [s, JSExpressionStatement e (JSSemi ann')] ann
  (<>) (JS (JSAstExpression e ann)) (JS (JSAstProgram ss _)) =
    JS $ JSAstProgram (JSExpressionStatement e (JSSemi ann) : ss) ann
  (<>) (JS (JSAstExpression e ann)) (JS (JSAstStatement s' _)) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann), s'] ann
  (<>) (JS (JSAstExpression e ann)) (JS (JSAstExpression e' ann')) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann), JSExpressionStatement e' (JSSemi ann')] ann
  (<>) (JS (JSAstExpression e ann)) (JS (JSAstLiteral e' ann')) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann), JSExpressionStatement e' (JSSemi ann')] ann
  (<>) (JS (JSAstLiteral e ann)) (JS (JSAstProgram ss _)) =
    JS $ JSAstProgram (JSExpressionStatement e (JSSemi ann) : ss) ann
  (<>) (JS (JSAstLiteral e ann)) (JS (JSAstStatement s' _)) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann), s'] ann
  (<>) (JS (JSAstLiteral e ann)) (JS (JSAstExpression e' ann')) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann), JSExpressionStatement e' (JSSemi ann')] ann
  (<>) (JS (JSAstLiteral e ann)) (JS (JSAstLiteral e' ann')) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann), JSExpressionStatement e' (JSSemi ann')] ann

instance Monoid JS where
  mempty = JS $ JSAstProgram [] (JSAnnot (TokenPn 0 0 0) [])

  mappend = (<>)

-- | Unifies javascript as 'JSStatement' and script as 'Text'.
data RepJs = RepJs JS | RepJsText Text deriving (Eq, Show, Generic)

instance Semigroup RepJs where
  (<>) (RepJs js) (RepJs js') = RepJs (js <> js')
  (<>) (RepJsText js) (RepJsText js') = RepJsText (js <> js')
  (<>) (RepJs js) (RepJsText js') =
    RepJsText (toStrict (renderToText $ unJS js) <> js')
  (<>) (RepJsText js) (RepJs js') =
    RepJsText (js <> toStrict (renderToText $ unJS js'))

instance Monoid RepJs where
  mempty = RepJs mempty

  mappend = (<>)

-- | Wrap js in standard DOM window loader.
onLoad :: RepJs -> RepJs
onLoad (RepJs js) = RepJs $ onLoadStatements [toStatement js]
onLoad (RepJsText js) = RepJsText $ onLoadText js

toStatement :: JS -> JSStatement
toStatement (JS (JSAstProgram ss ann)) = JSStatementBlock JSNoAnnot ss JSNoAnnot (JSSemi ann)
toStatement (JS (JSAstStatement s _)) = s
toStatement (JS (JSAstExpression e ann')) = JSExpressionStatement e (JSSemi ann')
toStatement (JS (JSAstLiteral e ann')) = JSExpressionStatement e (JSSemi ann')

onLoadStatements :: [JSStatement] -> JS
onLoadStatements js = JS $ JSAstProgram [JSAssignStatement (JSMemberDot (JSIdentifier JSNoAnnot "window") JSNoAnnot (JSIdentifier JSNoAnnot "onload")) (JSAssign JSNoAnnot) (JSFunctionExpression JSNoAnnot JSIdentNone JSNoAnnot JSLNil JSNoAnnot (JSBlock JSNoAnnot js JSNoAnnot)) JSSemiAuto] JSNoAnnot

onLoadText :: Text -> Text
onLoadText t = [qc| window.onload=function()\{{t}};|]

-- | Convert 'Text' to 'JS', throwing an error on incorrectness.
parseJs :: Text -> JS
parseJs = JS . readJs . unpack

-- | Render 'JS' as 'Text'.
renderJs :: JS -> Text
renderJs = toStrict . renderToText . unJS

-- | Render 'RepJs' as 'Text'.
renderRepJs :: PageRender -> RepJs -> Text
renderRepJs _ (RepJsText js) = js
renderRepJs Minified (RepJs js) = toStrict . renderToText . minifyJS . unJS $ js
renderRepJs Pretty (RepJs js) = toStrict . renderToText . unJS $ js
