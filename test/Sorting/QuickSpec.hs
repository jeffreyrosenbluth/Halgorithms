module Sorting.QuickSpec (spec) where

import qualified Data.List       as L
import qualified Data.Vector     as V
import           Sorting.Quick
import           Sorting.Sorting
import           Test.Hspec
import           Test.QuickCheck


spec :: Spec
spec =
  describe "Quick Sort" $ do
    it "Sorts a vector using quick sort." $
      sort (V.fromList "sortexample") `shouldBe` V.fromList "aeelmoprstx"
    it "Sorts a list of Ints." $ property $
      \x -> (sort . V.fromList) x == V.fromList (L.sort (x :: [Int]))
