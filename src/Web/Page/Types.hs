{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Web.Page.Types
  ( Page (..),
    PageConfig (..),
    defaultPageConfig,
    Concerns (..),
    suffixes,
    concernNames,
    PageConcerns (..),
    PageStructure (..),
    PageRender (..),
    Css (..),
    PageCss (..),
    renderCss,
    renderPageCss,
    JS (..),
    PageJs (..),
    onLoad,
    renderPageJs,
    parseJs,
    renderJs,
    Element (..),
    RepF (..),
    Rep,
    oneRep,
    SharedRepF (..),
    SharedRep,
    runOnce,
    zeroState,
  )
where

import qualified Clay
import Clay (Css (..))
import Control.Lens
import Control.Monad.Morph
import Data.Aeson
import Data.Biapplicative
import Data.Bifunctor (Bifunctor (..))
import Data.Generics.Labels ()
import Data.HashMap.Strict as HashMap hiding (foldr)
import Data.Semigroup ((<>))
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Data.Text.Lazy (toStrict)
import GHC.Generics hiding (Rep)
import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Language.JavaScript.Process.Minify
import Lucid
import Text.InterpolatedString.Perl6
import Prelude
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Applicative

-- | Components of a web page.
--
-- A web page typically can take many forms but still be the same web page.  For example, css can be linked to in a separate file, or can be inline within html, but still be the same css. This type represents the practical components of what makes up a web page.
data Page
  = Page
      { -- | css library links
        libsCss :: [Html ()],
        -- | javascript library links
        libsJs :: [Html ()],
        -- | css
        cssBody :: PageCss,
        -- | javascript with global scope
        jsGlobal :: PageJs,
        -- | javascript included within the onLoad function
        jsOnLoad :: PageJs,
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

-- | A web page typically is composed of css, javascript and html
--
-- 'Concerns' abstracts this compositional feature of a web page.
data Concerns a
  = Concerns
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

-- | the common file suffixes of the three concerns
suffixes :: Concerns FilePath
suffixes = Concerns ".css" ".js" ".html"

-- | create filenames for each Concern element.
concernNames :: FilePath -> FilePath -> Concerns FilePath
concernNames dir stem =
  (\x -> dir <> stem <> x) <$> suffixes

-- | Is the rendering to include all Concerns or be separated?
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

-- | Configuration of the rendering of a web page
data PageConfig
  = PageConfig
      { concerns :: PageConcerns,
        structure :: PageStructure,
        pageRender :: PageRender,
        filenames :: Concerns FilePath,
        localdirs :: [FilePath]
      }
  deriving (Show, Eq, Generic)

defaultPageConfig :: FilePath -> PageConfig
defaultPageConfig stem =
  PageConfig
    Inline
    HeaderBody
    Minified
    ((stem <>) <$> suffixes)
    []

-- | unifies css as a Clay.Css and css as Text
data PageCss = PageCss Clay.Css | PageCssText Text deriving (Generic)

instance Show PageCss where
  show (PageCss css) = unpack . renderCss $ css
  show (PageCssText txt) = unpack txt

instance Semigroup PageCss where
  (<>) (PageCss css) (PageCss css') = PageCss (css <> css')
  (<>) (PageCssText css) (PageCssText css') = PageCssText (css <> css')
  (<>) (PageCss css) (PageCssText css') =
    PageCssText (renderCss css <> css')
  (<>) (PageCssText css) (PageCss css') =
    PageCssText (css <> renderCss css')

instance Monoid PageCss where

  mempty = PageCssText mempty

  mappend = (<>)

-- | render PageCss as text
renderPageCss :: PageRender -> PageCss -> Text
renderPageCss Minified (PageCss css) = toStrict $ Clay.renderWith Clay.compact [] css
renderPageCss _ (PageCss css) = toStrict $ Clay.render css
renderPageCss _ (PageCssText css) = css

-- | render Css as text
renderCss :: Css -> Text
renderCss = toStrict . Clay.render

-- javascript types

-- | wrapper for JSAST
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

-- | unify JSStatement javascript and text-rendered script
data PageJs = PageJs JS | PageJsText Text deriving (Eq, Show, Generic)

instance Semigroup PageJs where
  (<>) (PageJs js) (PageJs js') = PageJs (js <> js')
  (<>) (PageJsText js) (PageJsText js') = PageJsText (js <> js')
  (<>) (PageJs js) (PageJsText js') =
    PageJsText (toStrict (renderToText $ unJS js) <> js')
  (<>) (PageJsText js) (PageJs js') =
    PageJsText (js <> toStrict (renderToText $ unJS js'))

instance Monoid PageJs where

  mempty = PageJs mempty

  mappend = (<>)

-- | wrap js in standard DOM window loader
onLoad :: PageJs -> PageJs
onLoad (PageJs js) = PageJs $ onLoadStatements [toStatement js]
onLoad (PageJsText js) = PageJsText $ onLoadText js

toStatement :: JS -> JSStatement
toStatement (JS (JSAstProgram ss ann)) = JSStatementBlock JSNoAnnot ss JSNoAnnot (JSSemi ann)
toStatement (JS (JSAstStatement s _)) = s
toStatement (JS (JSAstExpression e ann')) = JSExpressionStatement e (JSSemi ann')
toStatement (JS (JSAstLiteral e ann')) = JSExpressionStatement e (JSSemi ann')

onLoadStatements :: [JSStatement] -> JS
onLoadStatements js = JS $ JSAstProgram [JSAssignStatement (JSMemberDot (JSIdentifier JSNoAnnot "window") JSNoAnnot (JSIdentifier JSNoAnnot "onload")) (JSAssign JSNoAnnot) (JSFunctionExpression JSNoAnnot JSIdentNone JSNoAnnot JSLNil JSNoAnnot (JSBlock JSNoAnnot js JSNoAnnot)) JSSemiAuto] JSNoAnnot

onLoadText :: Text -> Text
onLoadText t = [qc| window.onload=function()\{{t}};|]

-- | convert text to JS
parseJs :: Text -> JS
parseJs = JS . readJs . Text.unpack

-- | render JS as text
renderJs :: JS -> Text
renderJs = toStrict . renderToText . unJS

-- | render PageJs as text
renderPageJs :: PageRender -> PageJs -> Text
renderPageJs _ (PageJsText js) = js
renderPageJs Minified (PageJs js) = toStrict . renderToText . minifyJS . unJS $ js
renderPageJs Pretty (PageJs js) = toStrict . renderToText . unJS $ js

-- | Abstracted message event element
data Element
  = Element
      { element :: Text,
        value :: Text
      }
  deriving (Eq, Show, Generic)

instance ToJSON Element

instance FromJSON Element where
  parseJSON = withObject "Element" $ \v ->
    Element
      <$> v .: "element"
      <*> v .: "value"

data RepF r a
  = Rep
      { rep :: r,
        make :: HashMap Text Text -> (HashMap Text Text, Either Text a)
      }
  deriving (Functor)

type Rep a = RepF (Html ()) a

instance (Semigroup r) => Semigroup (RepF r a) where
  (Rep r0 a0) <> (Rep r1 a1) =
    Rep
      (r0 <> r1)
      (\hm -> let (hm', x') = a0 hm in let (hm'', x'') = a1 hm' in (hm'', x' <> x''))

instance (Monoid a, Monoid r) => Monoid (RepF r a) where

  mempty = Rep mempty (,Right mempty)

  mappend = (<>)

instance Bifunctor RepF where
  bimap f g (Rep r a) = Rep (f r) (second (fmap g) . a)

instance Biapplicative RepF where

  bipure r a = Rep r (,Right a)

  (Rep fr fa) <<*>> (Rep r a) =
    Rep
      (fr r)
      ( \hm ->
          let (hm', a') = a hm in let (hm'', fa') = fa hm' in (hm'', fa' <*> a')
      )

instance (Monoid r) => Applicative (RepF r) where

  pure = bipure mempty

  Rep fh fm <*> Rep ah am =
    Rep
      (fh <> ah)
      ( \hm ->
          let (hm', a') = am hm in let (hm'', fa') = fm hm' in (hm'', fa' <*> a')
      )

oneRep :: (Monad m, MonadIO m) => Rep a -> (Rep a -> HashMap Text Text -> m ()) -> StateT (HashMap Text Text) m (HashMap Text Text, Either Text a)
oneRep r@(Rep _ fa) action = do
  m <- get
  let (m', a) = fa m
  put m'
  lift $ action r m'
  pure (m', a)

newtype SharedRepF m r a
  = SharedRep
      { unrep :: StateT (Int, HashMap Text Text) m (RepF r a)
      }
  deriving (Functor)

type SharedRep m a = SharedRepF m (Html ()) a

instance (Functor m) => Bifunctor (SharedRepF m) where
  bimap f g (SharedRep s) = SharedRep $ fmap (bimap f g) s

instance (Monad m) => Biapplicative (SharedRepF m) where

  bipure r a = SharedRep $ pure $ bipure r a

  (SharedRep f) <<*>> (SharedRep a) = SharedRep $ liftA2 (<<*>>) f a

instance (Monad m, Monoid r) => Applicative (SharedRepF m r) where

  pure = bipure mempty

  SharedRep f <*> SharedRep a = SharedRep $ liftA2 (<*>) f a

-- | compute the initial state of a SharedRep (testing)
zeroState ::
  (Monad m) =>
  SharedRep m a ->
  m (Html (), (HashMap Text Text, Either Text a))
zeroState sr = do
  (Rep h fa, (_, m)) <- flip runStateT (0, HashMap.empty) $ unrep sr
  pure (h, fa m)

-- | compute the initial state of a SharedRep and then run a single action (testing)
runOnce ::
  (Monad m) =>
  SharedRep m a ->
  (Html () -> HashMap Text Text -> m ()) ->
  m (HashMap Text Text, Either Text a)
runOnce sr action = do
  (Rep h fa, (_, m)) <- flip runStateT (0, HashMap.empty) $ unrep sr
  action h m
  pure (fa m)
-- renderHtml :: Html a -> Text
-- renderHtml = toText
