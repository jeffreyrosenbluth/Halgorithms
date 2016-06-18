{-# LANGUAGE MultiWayIf       #-}

----------------------------------------------------------
-- |
-- 2 Sorting
-- Selection sort, Inserting sort, Shellsort, Megesort,
-- Bottum up mergesort and QuickSort.
-- Transcription of "http://algs4.cs.princeton.edu".
-- (c) 2014 Jeffrey Rosenbluth
----------------------------------------------------------

module Sorting.Merge where

import           Common.References

import           Control.Monad
import           Control.Monad.ST
import           Data.STRef
import           Data.Vector.Generic         (Vector, unsafeThaw, unsafeFreeze)
import           Data.Vector.Generic.Mutable (read, write, swap, MVector)
import qualified Data.Vector.Generic.Mutable as VM
import           Prelude                     hiding (read)

-- | For merge sort and bottom up merge sort, Algorithms 2.4.
--   Merge 'arr[lo..mid]' with 'arr[mid+1..hi].
merge :: (Ord a, MVector v a) => v s a -> Int -> Int -> Int -> ST s ()
merge vec lo mid hi = do
    iRef <- newSTRef lo
    jRef <- newSTRef (mid + 1)
    -- Allocate space just once and copy the array.
    aux  <- VM.clone vec
    forM_ [lo..hi] $ \k -> do
      i    <- readSTRef iRef
      j    <- readSTRef jRef
      auxi <- read aux (min i hi)
      auxj <- read aux (min j hi)
      -- Merge back to 'vec[lo..hi]
      if | i > mid -> do
             write vec k =<< read aux j
             jRef .= j + 1
         | j > hi -> do
             write vec k =<< read aux i
             iRef .= i + 1
         | auxj < auxi -> do
             write vec k =<< read aux j
             jRef .= j + 1
         | otherwise -> do
             write vec k =<< read aux i
             iRef .= i + 1

-- | Top-down mergesort, Algorithm 2.4
sort :: (Ord a, Vector v a) => v a -> v a
sort arr = runST $ do
  vec <- unsafeThaw arr
  let hi = VM.length vec - 1
      sort l h = when (l < h) $ do
        let m = l + (h - l) `div` 2
        sort l m           -- Sort left half.
        sort (m + 1) h     -- Sort right half.
        merge vec l m h -- Merge results.
  sort 0 hi
  unsafeFreeze vec

-- | Bottom-up mergesort.
sortBU :: (Ord a, Vector v a) => v a -> v a
sortBU arr = runST $ do
  vec <- unsafeThaw arr
  let hi = VM.length vec
  -- Do log hi passes of pairwise merges.
  -- sz: subarray size
  -- i:  subarray index
  forM_ (takeWhile (< hi) [2 ^ i | i <- [(0 :: Int)..]]) $ \sz ->
    forM_ [0, sz + sz .. hi - sz - 1] $ \i ->
      merge vec i (i + sz - 1) (min (i + sz + sz - 1) (hi - 1))
  unsafeFreeze vec
