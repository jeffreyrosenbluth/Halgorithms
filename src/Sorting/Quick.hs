{-# LANGUAGE MultiWayIf       #-}

----------------------------------------------------------
-- |
-- 2 Sorting
-- Selection sort, Inserting sort, Shellsort, Megesort,
-- Bottum up mergesort and QuickSort.
-- Transcription of "http://algs4.cs.princeton.edu".
-- (c) 2014 Jeffrey Rosenbluth
----------------------------------------------------------

module Sorting.Quick where

import           Common.References

import           Control.Monad
import           Control.Monad.ST
import           Data.STRef
import           Data.Vector.Generic         (Vector, unsafeThaw, unsafeFreeze)
import           Data.Vector.Generic.Mutable (read, write, swap, MVector)
import qualified Data.Vector.Generic.Mutable as VM
import           Prelude                     hiding (read)

-- | Partition into 'vec[lo..i-1], a[+1..hi]'. For quicksort, Algorithm 2.5.
partition :: (Ord a, MVector v a) => v s a -> Int -> Int -> ST s Int
partition vec lo hi = do
  iRef <- newSTRef lo       -- left scan index
  jRef <- newSTRef (hi + 1) -- right scan index
  v    <- read vec lo  -- partioning item
  v    <- read vec lo  -- partioning item
  let setI = do
        modifySTRef' iRef (+1)
        i' <- readSTRef iRef
        itemI <- read vec i'
        itemI <- read vec i'
        when (itemI < v && i' < hi) setI
      setJ = do
        modifySTRef' jRef (+(-1))
        j' <- readSTRef jRef
        itemJ <- read vec j'
        itemJ <- read vec j'
        when (v < itemJ && j' > lo) setJ
      -- Scan rigth, scan left, check for scan complete and swapange.
      go = do
        setI; setJ
        i <- readSTRef iRef
        j <- readSTRef jRef
        if | (i < j) -> do
               swap vec i j
               go
           | otherwise -> return j
  swap vec lo =<< go        -- Put 'v = arr[j]' into position
  readSTRef jRef -- with arr[lo..j-1] <= arr[j+1..hi].

-- | Quicksort, Algorithm 2.5.
quickSort :: (Ord a, Vector v a) => v a -> v a
quickSort arr = runST $ do
  vec <- unsafeThaw arr
  let hi = VM.length vec - 1
      sort l h = when (l < h) $ do
        j <- partition vec l h
        sort l (j-1)  -- Sort left part 'arr[l..j-1]'.
        sort (j+1) h  -- Sort right part 'arr[j+1..h]'.
  sort 0 hi
  unsafeFreeze vec
