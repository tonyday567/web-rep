{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Web.Page.Js
  ( JS(..)
  , JSStatement(..)
  , PageJs(..)
  , minifyJS
  , onLoad
  , onLoadStatements
  , toStatements
  , toStatement
  , renderToStatement
  , renderToText
  , readJs
  , fromText
  ) where

import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Language.JavaScript.Process.Minify
import Protolude hiding ((<>))
import qualified Data.Text as Text
import Data.Semigroup ((<>))
import Text.InterpolatedString.Perl6

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
    JS $ JSAstProgram [s,s'] ann
  (<>) (JS (JSAstStatement s ann)) (JS (JSAstExpression e ann')) =
    JS $ JSAstProgram [s,JSExpressionStatement e (JSSemi ann')] ann
  (<>) (JS (JSAstStatement s ann)) (JS (JSAstLiteral e ann')) =
    JS $ JSAstProgram [s,JSExpressionStatement e (JSSemi ann')] ann
  (<>) (JS (JSAstExpression e ann)) (JS (JSAstProgram ss _)) =
    JS $ JSAstProgram (JSExpressionStatement e (JSSemi ann) : ss) ann
  (<>) (JS (JSAstExpression e ann)) (JS (JSAstStatement s' _)) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann),s'] ann
  (<>) (JS (JSAstExpression e ann)) (JS (JSAstExpression e' ann')) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann),JSExpressionStatement e' (JSSemi ann')] ann
  (<>) (JS (JSAstExpression e ann)) (JS (JSAstLiteral e' ann')) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann),JSExpressionStatement e' (JSSemi ann')] ann
  (<>) (JS (JSAstLiteral e ann)) (JS (JSAstProgram ss _)) =
    JS $ JSAstProgram (JSExpressionStatement e (JSSemi ann) : ss) ann
  (<>) (JS (JSAstLiteral e ann)) (JS (JSAstStatement s' _)) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann),s'] ann
  (<>) (JS (JSAstLiteral e ann)) (JS (JSAstExpression e' ann')) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann),JSExpressionStatement e' (JSSemi ann')] ann
  (<>) (JS (JSAstLiteral e ann)) (JS (JSAstLiteral e' ann')) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann),JSExpressionStatement e' (JSSemi ann')] ann


instance Monoid JS where
  mempty = JS $ JSAstProgram [] (JSAnnot (TokenPn 0 0 0) [])
  mappend = (<>)

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

toStatements :: JS -> [JSStatement]
toStatements (JS (JSAstProgram ss _)) = ss
toStatements (JS (JSAstStatement s _)) = [s]
toStatements (JS (JSAstExpression e ann')) = [JSExpressionStatement e (JSSemi ann')]
toStatements (JS (JSAstLiteral e ann')) = [JSExpressionStatement e (JSSemi ann')]

toStatement :: JS -> JSStatement
toStatement (JS (JSAstProgram ss ann)) = JSStatementBlock JSNoAnnot ss JSNoAnnot (JSSemi ann)
toStatement (JS (JSAstStatement s _)) = s
toStatement (JS (JSAstExpression e ann')) = JSExpressionStatement e (JSSemi ann')
toStatement (JS (JSAstLiteral e ann')) = JSExpressionStatement e (JSSemi ann')

renderToStatement :: Text -> JSStatement
renderToStatement t = toStatement $ JS $ readJs $ Text.unpack t

-- | standard window loader
onLoadStatements :: [JSStatement] -> JS
onLoadStatements js = JS $ JSAstProgram [JSAssignStatement (JSMemberDot (JSIdentifier JSNoAnnot "window") JSNoAnnot (JSIdentifier JSNoAnnot "onload")) (JSAssign JSNoAnnot) (JSFunctionExpression JSNoAnnot JSIdentNone JSNoAnnot JSLNil JSNoAnnot (JSBlock JSNoAnnot js JSNoAnnot)) JSSemiAuto] JSNoAnnot

onLoadText :: Text -> Text
onLoadText t = [qc| window.onload=function()\{{t}};|]

onLoad :: PageJs -> PageJs
onLoad (PageJs js) = PageJs $ onLoadStatements [toStatement js]
onLoad (PageJsText js) = PageJsText $ onLoadText js

fromText :: Text -> JS
fromText = JS . readJs . Text.unpack
