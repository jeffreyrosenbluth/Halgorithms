module Sorting.InsertionSpec (spec) where

import Test.Hspec
import Sorting.Insertion
import Data.Vector as V

spec :: Spec
spec =
  describe "Insertion Sort" $
    it "sorts a vector using insertin sort" $
      sort (V.fromList "sortexample") `shouldBe` V.fromList "aeelmoprstx"
