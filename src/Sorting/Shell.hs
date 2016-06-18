{-# LANGUAGE MultiWayIf       #-}

----------------------------------------------------------
-- |
-- 2 Sorting
-- Selection sort, Inserting sort, Shellsort, Megesort,
-- Bottum up mergesort and QuickSort.
-- Transcription of "http://algs4.cs.princeton.edu".
-- (c) 2014 Jeffrey Rosenbluth
----------------------------------------------------------

module Sorting.Shell where

import           Common.References

import           Control.Monad
import           Control.Monad.ST
import           Data.STRef
import           Data.Vector.Generic         (Vector, unsafeThaw, unsafeFreeze)
import           Data.Vector.Generic.Mutable (read, write, swap, MVector)
import qualified Data.Vector.Generic.Mutable as VM
import           Prelude                     hiding (read)

-- | Shellsort, Algorithm 2.3
sort :: (Ord a, Vector v a) => v a -> v a
sort arr = runST $ do
  vec <- unsafeThaw arr
  let n      = VM.length vec - 1
      hs     = reverse . takeWhile (<= n `div` 3 + 1) $ iterate (\x -> 3 * x + 1) 1
      go h j = when (j >= h) $ do
        b <- read vec j
        a <- read vec (j - h)
        when (b < a) $ do
          swap vec j (j - h)
          go h (j - h)
  forM_ hs $ \h' -> forM_ [h'..n] (go h')
  unsafeFreeze vec
