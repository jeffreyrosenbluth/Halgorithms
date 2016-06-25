{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
----------------------------------------------------------
-- |
-- 2 Sorting
-- Selection sort, Inserting sort, Shellsort, Megesort,
-- Bottum up mergesort and QuickSort.
-- Transcription of "http://algs4.cs.princeton.edu".
-- (c) 2014 Jeffrey Rosenbluth
----------------------------------------------------------

module Sorting.MergeBU where

import           Common.References
import           Sorting.Sorting
import           Control.Monad               (forM_)
import           Control.Monad.ST            (ST)
import           Data.Vector.Generic         (Vector)
import           Data.Vector.Generic.Mutable (MVector, length)
import           Prelude                     hiding (length)

-- | Bottom-up mergesort. For mutable vectors
sortBy' :: MVector v a => (a -> a -> Ordering) -> v s a -> ST s ()
sortBy' cmp vec = do
  let hi = length vec
  -- Do log hi passes of pairwise merges, sz: subarray size, i:  subarray index.
  forM_ (takeWhile (< hi) [2 ^ i | i <- [(0 :: Int)..]]) $ \sz ->
    forM_ [0, sz + sz .. hi - sz - 1] $ \i ->
      merge cmp vec i (i + sz - 1) (min (i + sz + sz - 1) (hi - 1))

sort' :: (Ord a, MVector v a) => v s a -> ST s ()
sort' = sortBy' compare

sortBy :: (Vector v a) => (a -> a -> Ordering) -> v a -> v a
sortBy = toImmutable sortBy'

sort :: (Ord a, Vector v a) => v a -> v a
sort = sortBy compare

sortOn :: (Ord b, Vector v a, Vector v (b, a), Functor v) => (a -> b) -> v a -> v a
sortOn = mkSortOn sortBy'
