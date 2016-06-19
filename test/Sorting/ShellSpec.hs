module Sorting.ShellSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Sorting.Shell
import qualified Data.Vector as V
import qualified Data.List as L

spec :: Spec
spec =
  describe "Shell Sort" $ do
    it "Sorts a vector using shell sort." $
      sort (V.fromList "sortexample") `shouldBe` V.fromList "aeelmoprstx"
    it "Sorts a list of Ints." $ property $
      \x -> (sort . V.fromList) x == V.fromList (L.sort (x :: [Int]))
