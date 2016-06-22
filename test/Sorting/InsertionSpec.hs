module Sorting.InsertionSpec (spec) where

import qualified Data.List         as L
import qualified Data.Vector       as V
import           Sorting.Insertion
import           Sorting.Sorting
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
  describe "Insertion Sort" $ do
    it "Sorts a vector using insertion sort." $
      sort (V.fromList "sortexample") `shouldBe` V.fromList "aeelmoprstx"
    it "Sorts a list of Ints." $ property $
      \x -> (sort . V.fromList) x == V.fromList (L.sort (x :: [Int]))
    it "Insertion sort is stable." $ property $
      \x -> (sortOn (`mod` 3) . V.fromList) x == V.fromList (L.sortOn (`mod` 3) (x :: [Int]))
