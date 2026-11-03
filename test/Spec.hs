module Main (main) where

import qualified CategorizeSpec
import qualified JsonSpec
import qualified RulesSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  CategorizeSpec.spec
  RulesSpec.spec
  JsonSpec.spec
