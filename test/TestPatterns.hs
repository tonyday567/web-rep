{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module TestPatterns where

import Control.Applicative
import Data.Monoid
import qualified Data.Map as Map
import Web.Page

modCounter :: JExpr
modCounter = 
  module' 
  [] 
  []
  [var' "i" (0::Int)]
  $ Map.fromList 
  [ ("get", fun' [] [] (ref "i"))
  , ("set", fun' ["val"] [[jmacro|i=val;|]] (ref "null"))
  , ("increment", fun' [] [] [jmacroE|++i|])
  ]

modCounter' :: JExpr
modCounter' = 
  module' [] [] 
  [ var' "i" (0::Int) 
  , (ref "get" =: fun' [] [] (ref "i"))
  , (ref "set" =: fun' ["val"] [[jmacro|i=val;|]] (ref "null"))
  , (ref "increment" =: fun' [] [] [jmacroE|++i|])
  ]
  $ Map.fromList 
  [ ("get", ref "get")
  , ("set", ref "set")
  , ("increment", ref "increment")
  ]

modCounter'' :: JExpr
modCounter'' = 
  module' 
  ["i"]
  [0]
  []
  $ Map.fromList 
  [ ("get", fun' [] [] (ref "i"))
  , ("set", fun' ["val"] [[jmacro|i=val;|]] (ref "null"))
  , ("increment", fun' [] [] [jmacroE|++i|])
  ]

-- http://benalman.com/news/2010/11/immediately-invoked-function-expression/
tCounter :: JStat
tCounter = 
  [jmacro|
   var counter = `(modCounter)`;
   
   QUnit.test( "module pattern", function( assert ) {
     assert.ok( 0 == counter.get(), "get ok" );
     counter.set( 3);
     assert.ok( 3 == counter.get(), "set ok" );
     counter.increment();
     assert.ok( 4 == counter.get(), "increment ok" );
   });
   |]

tCounter' :: JStat
tCounter' = 
  [jmacro|
   var counter2 = `(modCounter')`;
   
   QUnit.test( "module - functions as local declarations", function( assert ) {
     assert.ok( 0 == counter2.get(), "get ok" );
     counter2.set( 3);
     assert.ok( 3 == counter2.get(), "set ok" );
     counter2.increment();
     assert.ok( 4 == counter2.get(), "increment ok" );
   });
   |]

tCounter'' :: JStat
tCounter'' = 
  [jmacro|
   var counter3 = `(modCounter'')`;
   
   QUnit.test( "module - passing in initial values", function( assert ) {
     assert.ok( 0 == counter3.get(), "get ok" );
     counter3.set( 3);
     assert.ok( 3 == counter3.get(), "set ok" );
     counter3.increment();
     assert.ok( 4 == counter3.get(), "increment ok" );
   });
   |]


tPage = 
  Page 
  ["http://code.jquery.com/qunit/qunit-1.17.1.css"] 
  ["http://code.jquery.com/qunit/qunit-1.17.1.js"] 
  mempty 
  (tCounter <> tCounter' <> tCounter'')
  mempty 
  ( meta_ [charset_ "utf-8"] <> 
    title_ "web-page testing") 
  ( div_  [id_ "qunit"] mempty <> 
    div_ [id_ "qunit-fixture"] mempty)


tPageConfig = PageConfig Separated Headless Pretty LinkedLibs (Concerns "test.css" "test.js" "test.html")

tFile = renderPageToFileConcernsWith' tPageConfig tPage (Concerns mempty "test.js" "test.html")

