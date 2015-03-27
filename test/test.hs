{-

hdcharts contains many moving parts that will undergo quite extensive innovation, be abandoned, or fall out of favour compared with other stuff. Any test framework thus runs the risk of becoming out of date, being a waste or just over-the-top structure.  Test-driven, however, seems like a vital ingredient given the non-haskell moving parts involved, and thus the inability, more generally, of employing equational reasoning.  Hence this prelude to the tests, and hence a rather loose collection of tests below ...

-}

module Main where

import qualified TestChain

import           Control.Applicative
import           Test.Tasty
import           Test.Tasty.Hspec

-- The tests
tests :: IO TestTree
tests = testGroup "the tests" <$> sequence
  [ testSpec "Language.Javascript.JMacro.Chain" =<< TestChain.testsChain
  ]

main :: IO ()
main = defaultMain =<< tests
