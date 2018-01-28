{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE QuasiQuotes #-}

-- some jmacro combinators and helpers
-- could also be called Language.Javascript.JMacro.Extended
module Web.Page.Js
  ( module Web.Page.Js
  , module Language.Javascript.JMacro
  , module Language.Javascript.JMacro.Chain
  ) where

import           Blaze.ByteString.Builder (toLazyByteString)
import           Control.Applicative
import Data.ByteString.Lazy (unpack)
import           Data.Monoid
import           GHC.Char (chr)
import           Language.JavaScript.Parser (renderJS, readJs)
import           Language.Javascript.JMacro
import           Language.Javascript.JMacro.Chain
import           Text.Jasmine (minify)
import qualified Data.Map as Map
-- import Data.Maybe

-- | a javascript reference
ref :: String -> JExpr
ref = ValExpr . JVar . StrI

-- | a javascript literal
lit :: String -> JExpr
lit = ValExpr . JStr

-- | a var declaration
var :: String -> JStat
var s = DeclStat (StrI s) Nothing

-- | a var declaration and assignment
var' :: (ToJExpr a) => String -> a -> JStat
var' s val =
  BlockStat 
  [ DeclStat (StrI s) Nothing
  , AssignStat (ValExpr (JVar (StrI s))) (toJExpr val)
  ]

-- | an object declaration
varObj :: String -> JStat
varObj s = var s <> (ref s =: ref "{}")

-- | javascript assignment
infixl 2 =:
(=:) :: ToJExpr a => JExpr -> a -> JStat
x =:  y = AssignStat x (toJExpr y)

-- | apply an argument list to a function
apply :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
apply x y = ApplExpr (toJExpr x) (toJExprList y)

-- | apply an argument list to a function
apply' :: (ToJExpr a, ToJExpr b) => a -> b -> JStat
apply' x y = ApplStat (toJExpr x) (toJExprList y)

toJExprList :: ToJExpr a => a -> [JExpr]
toJExprList x = case toJExpr x of
                  (ValExpr (JList l)) -> l
                  x' -> [x']

-- | fixme: anonymous function returning mutated parameter
anonMutate :: String -> JStat -> JExpr
anonMutate c stats = ValExpr (JFunc [StrI c] (stats <> ReturnStat (ref c)))

-- | anonymous function taking 2 parameters and returning first (mutated) parameter
anonMutate2 :: String -> String -> JStat -> JExpr
anonMutate2 c d stats  = ValExpr (JFunc [StrI c,StrI d] (stats <> ReturnStat (ref c)))

-- | render jmacro javascript to a String value
-- this can fail due to passing through readJs (but picking up a large class of bugs) 
render :: JStat -> String
render = Prelude.map (chr . fromIntegral) . unpack . toLazyByteString . renderJS . readJs . show . renderJs

-- | render jmacro javascript to a minified String value 
renderMin :: JStat -> String
renderMin = Prelude.map (chr . fromIntegral) . unpack . minify . toLazyByteString . renderJS . readJs . show . renderJs

-- | standard window loader
onLoad :: JStat -> JStat
onLoad es = ref "window.onload" =: fun0 es

-- | anonymous function
fun :: [String] -> JStat -> JExpr
fun args ss = ValExpr (JFunc (StrI <$> args) ss) 

-- | anonymous function with return statement as a separate parameter
fun' :: (ToJExpr a) => [String] -> [JStat] -> a -> JExpr
fun' args ss r = fun args (BlockStat $ ss <> [ReturnStat . toJExpr $ r]) 

-- | anonymous function taking 0 parameters
fun0 :: JStat -> JExpr
fun0 ss = fun [] ss

-- | anonymous function taking a single parameter
fun1 :: String -> JStat -> JExpr
fun1 arg ss = fun [arg] ss

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
moduleS :: String -> [String] -> [JExpr] -> [JStat] -> Map.Map String JExpr -> JStat
moduleS name lvars lvals lstats obj = var' name (moduleE lvars lvals lstats obj)

-- | anonymous function returning mutated parameter
mutate :: JStat -> JExpr
mutate ss = ValExpr (JFunc [StrI "x"] (ss <> ReturnStat (ref "x")))

-- | anonymous function taking 2 parameters and returning first (mutated) parameter
mutate' :: String -> JStat -> JExpr
mutate' arg ss  = ValExpr (JFunc [StrI "x",StrI arg] (ss <> ReturnStat (ref "x")))

-- | mutate method, passing in the module name as a parameter
-- http://www.adequatelygood.com/JavaScript-Module-Pattern-In-Depth.html
moduleL :: String -> JStat -> JStat
moduleL name ss = var' name (ApplExpr (mutate ss) [[jmacroE|`(ref name)` || {}|]])

moduleL' :: String -> String -> JStat -> JStat
moduleL' name arg ss = var' name (ApplExpr (mutate' arg ss) [[jmacroE|`(ref name)` || {}|]])

-- https://github.com/rwaldron/idiomatic.js
moduleP :: String-> [String] -> [JExpr] -> [JStat] -> Map.Map String JExpr -> JStat
moduleP name lvars lvals lstats obj = 
  ApplStat 
  ( fun ["global"] 
    ( moduleS name lvars lvals lstats obj <> 
      [jmacro|
       `(ref $ "global."<>name)` = `(name)`;
      |]))
  [ref "this"]

