module Sorting.SelectionSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Sorting.Selection
import qualified Data.Vector as V
import qualified Data.List as L


spec :: Spec
spec =
  describe "Seclection Sort" $ do
    it "Sorts a vector using selction sort" $
      sort (V.fromList "sortexample") `shouldBe` V.fromList "aeelmoprstx"
    it "Sorts a list of Ints." $ property $
      \x -> (sort . V.fromList) x == V.fromList (L.sort (x :: [Int]))
