module Main where

import qualified TestChain
import qualified TestRender 

import           Control.Applicative
import           Test.Tasty
import           Test.Tasty.Hspec

-- The tests
tests :: IO TestTree
tests = testGroup "the tests" <$> sequence
  [ testSpec "Language.Javascript.JMacro.Chain" =<< TestChain.testsChain
  , testSpec "Lucid.Page.Render" =<< TestRender.testsRender
  ]

main :: IO ()
main = defaultMain =<< tests
