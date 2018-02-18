{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Web.Page.Js
  ( JS(..)
  , module Language.JavaScript.Parser
  , minifyJS
  , onLoad
  , toStatements
  , toStatement
  , renderToStatement
  ) where

import Language.JavaScript.Parser
import Language.JavaScript.Parser.AST
import Language.JavaScript.Process.Minify
import Protolude
import qualified Data.Text as Text

newtype JS = JS {unJS :: JSAST} deriving (Show, Eq, Generic)

instance Monoid JS where
  mempty = JS $ JSAstProgram [] (JSAnnot (TokenPn 0 0 0) [])
  mappend (JS (JSAstProgram ss ann)) (JS (JSAstProgram ss' _)) =
    JS $ JSAstProgram (ss <> ss') ann
  mappend (JS (JSAstProgram ss ann)) (JS (JSAstStatement s _)) =
    JS $ JSAstProgram (ss <> [s]) ann
  mappend (JS (JSAstProgram ss ann)) (JS (JSAstExpression e ann')) =
    JS $ JSAstProgram (ss <> [JSExpressionStatement e (JSSemi ann')]) ann
  mappend (JS (JSAstProgram ss ann)) (JS (JSAstLiteral e ann')) =
    JS $ JSAstProgram (ss <> [JSExpressionStatement e (JSSemi ann')]) ann
  mappend (JS (JSAstStatement s ann)) (JS (JSAstProgram ss _)) =
    JS $ JSAstProgram (s : ss) ann
  mappend (JS (JSAstStatement s ann)) (JS (JSAstStatement s' _)) =
    JS $ JSAstProgram [s,s'] ann
  mappend (JS (JSAstStatement s ann)) (JS (JSAstExpression e ann')) =
    JS $ JSAstProgram [s,JSExpressionStatement e (JSSemi ann')] ann
  mappend (JS (JSAstStatement s ann)) (JS (JSAstLiteral e ann')) =
    JS $ JSAstProgram [s,JSExpressionStatement e (JSSemi ann')] ann
  mappend (JS (JSAstExpression e ann)) (JS (JSAstProgram ss _)) =
    JS $ JSAstProgram (JSExpressionStatement e (JSSemi ann) : ss) ann
  mappend (JS (JSAstExpression e ann)) (JS (JSAstStatement s' _)) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann),s'] ann
  mappend (JS (JSAstExpression e ann)) (JS (JSAstExpression e' ann')) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann),JSExpressionStatement e' (JSSemi ann')] ann
  mappend (JS (JSAstExpression e ann)) (JS (JSAstLiteral e' ann')) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann),JSExpressionStatement e' (JSSemi ann')] ann
  mappend (JS (JSAstLiteral e ann)) (JS (JSAstProgram ss _)) =
    JS $ JSAstProgram (JSExpressionStatement e (JSSemi ann) : ss) ann
  mappend (JS (JSAstLiteral e ann)) (JS (JSAstStatement s' _)) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann),s'] ann
  mappend (JS (JSAstLiteral e ann)) (JS (JSAstExpression e' ann')) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann),JSExpressionStatement e' (JSSemi ann')] ann
  mappend (JS (JSAstLiteral e ann)) (JS (JSAstLiteral e' ann')) =
    JS $ JSAstProgram [JSExpressionStatement e (JSSemi ann),JSExpressionStatement e' (JSSemi ann')] ann

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
onLoad :: [JSStatement] -> JS
onLoad js = JS $ JSAstProgram [JSAssignStatement (JSMemberDot (JSIdentifier JSNoAnnot "window") JSNoAnnot (JSIdentifier JSNoAnnot "onload")) (JSAssign JSNoAnnot) (JSFunctionExpression JSNoAnnot JSIdentNone JSNoAnnot JSLNil JSNoAnnot (JSBlock JSNoAnnot js JSNoAnnot)) JSSemiAuto] JSNoAnnot

{-
-- | anonymous function
fun :: [String] -> JStat -> JExpr
fun args ss = ValExpr (JFunc (StrI <$> args) ss)

-- | anonymous function with return statement as a separate parameter
fun' :: (ToJExpr a) => [String] -> [JStat] -> a -> JExpr
fun' args ss r = fun args (BlockStat $ ss <> [ReturnStat . toJExpr $ r])

-- | anonymous function taking 0 parameters
fun0 :: JStat -> JExpr
fun0 = fun []

-- | anonymous function taking a single parameter
fun1 :: String -> JStat -> JExpr
fun1 arg = fun [arg]

-- | a closure
closure :: [JStat] -> JStat
closure ss = ApplStat (fun [] (BlockStat ss)) []

-- | a closure with parameters
closure' :: [String] -> [JStat] -> JStat
closure' args ss = ApplStat (fun args (BlockStat ss)) (ref <$> args)

-- | a module pattern - closure + returning an object
moduleE :: [String] -> [JExpr] -> [JStat] -> Map.Map String JExpr -> JExpr
moduleE lvars lvals stats obj =
  ApplExpr (fun' lvars stats (ValExpr (JHash obj))) lvals

-- | module pattern - JStat declaration
moduleS ::
     String -> [String] -> [JExpr] -> [JStat] -> Map.Map String JExpr -> JStat
moduleS name lvars lvals lstats obj = var' name (moduleE lvars lvals lstats obj)

-- | anonymous function returning mutated parameter
mutate :: JStat -> JExpr
mutate ss = ValExpr (JFunc [StrI "x"] (ss <> ReturnStat (ref "x")))

-- | anonymous function taking 2 parameters and returning first (mutated) parameter
mutate' :: String -> JStat -> JExpr
mutate' arg ss =
  ValExpr (JFunc [StrI "x", StrI arg] (ss <> ReturnStat (ref "x")))

-- | mutate method, passing in the module name as a parameter
-- http://www.adequatelygood.com/JavaScript-Module-Pattern-In-Depth.html
moduleL :: String -> JStat -> JStat
moduleL name ss =
  var' name (ApplExpr (mutate ss) [[jmacroE|`(ref name)` || {}|]])

moduleL' :: String -> String -> JStat -> JStat
moduleL' name arg ss =
  var' name (ApplExpr (mutate' arg ss) [[jmacroE|`(ref name)` || {}|]])

-- https://github.com/rwaldron/idiomatic.js
moduleP ::
     String -> [String] -> [JExpr] -> [JStat] -> Map.Map String JExpr -> JStat
moduleP name lvars lvals lstats obj =
  ApplStat
    (fun
       ["global"]
       (moduleS name lvars lvals lstats obj <>
        [jmacro|
       `(ref $ "global."<>name)` = `(name)`;
      |]))
    [ref "this"]

-}
