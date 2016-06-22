module Sorting.MergeBUSpec (spec) where

import qualified Data.List       as L
import qualified Data.Vector     as V
import           Sorting.MergeTD
import           Sorting.Sorting
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
  describe "Top-down merge Sort" $ do
    it "Sorts a vector using bottom-up merge sort." $
      sort (V.fromList "sortexample") `shouldBe` V.fromList "aeelmoprstx"
    it "Sorts a list of Ints." $ property $
      \x -> (sort . V.fromList) x == V.fromList (L.sort (x :: [Int]))
