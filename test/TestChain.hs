{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module TestChain where

import Control.Applicative
import Data.Monoid
import Test.Tasty.Hspec
import Lucid.Page.Js

trip :: JExpr -> Bool
trip = (==) <*> (toJExpr . toChain)

testsChain :: IO (SpecWith())
testsChain =
  return $ describe "Chain" $ do
    it "toJExpr . toChain round trip for common patterns" $ 
      and (trip <$>
      [ [jmacroE|a|]
      , [jmacroE|a(b)|]
      , [jmacroE|a.b.c|]
      , [jmacroE|a(b).c|]
      , [jmacroE|a(b).c(d)|]
      ])
      `shouldBe` True
    it "mappend works the right way round" $ 
      toJExpr (toChain (jsv "a") <> toChain (jsv "b")) `shouldBe` [jmacroE|a.b|]
    it "mconcat works too" $ 
      toJExpr (mconcat (fmap (toChain . jsv . (:[])) ['a'..'d']))
      `shouldBe` [jmacroE|a.b.c.d|]
    it "and typical d3-style expressions are sweet" $
      exChain `shouldBe` exExpr

-- | example
exExpr :: JExpr
exExpr =
  [jmacroE|
   d3.select("body").
   style("color", "black").
   style("background-color", "white")
  |]

line1 :: JExpr
line1 =
  [jmacroE|
   d3.select("body")
  |]

line2 :: JExpr
line2 =
  [jmacroE|
   style("color", "black")
  |]

line3 :: JExpr
line3 =
  [jmacroE|
   style("background-color", "white")
  |]

exChain :: JExpr
exChain =
  toJExpr $
  toChain line1 <>
  toChain line2 <>
  toChain line3
