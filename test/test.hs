module Main where

-- import Protolude
import qualified TestRender 
import qualified TestServe

import           Test.Tasty
import           Test.Tasty.Hspec

-- The tests
tests :: IO TestTree
tests = testGroup "the tests" <$> sequence
  [ testSpec "Web.Page.Render" =<< TestRender.testsRender
  ]

main :: IO ()
main = defaultMain =<< tests
