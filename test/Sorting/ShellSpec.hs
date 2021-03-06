module Sorting.ShellSpec (spec) where

import qualified Data.List       as L
import qualified Data.Vector     as V
import           Sorting.Shell
import           Sorting.Sorting
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec =
  describe "Shell Sort" $ do
    it "Sorts a vector using shell sort." $
      sort (V.fromList "sortexample") `shouldBe` V.fromList "aeelmoprstx"
    it "Sorts a list of Ints." $ property $
      \x -> (sort . V.fromList) x == V.fromList (L.sort (x :: [Int]))
