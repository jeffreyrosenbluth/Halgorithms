module Sorting.Quick3Spec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Sorting.Quick3
import qualified Data.Vector as V
import qualified Data.List as L

spec :: Spec
spec =
  describe "Insertion Sort" $ do
    it "Sorts a vector using 3 way quick sort." $
      sort (V.fromList "sortexample") `shouldBe` V.fromList "aeelmoprstx"
    it "Sorts a list of Ints." $ property $
      \x -> (sort . V.fromList) x == V.fromList (L.sort (x :: [Int]))
