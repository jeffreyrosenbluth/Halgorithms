{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}

----------------------------------------------------------
-- |
-- 2 Sorting
-- Selection sort, Inserting sort, Shellsort, Megesort,
-- Bottum up mergesort and QuickSort.
-- Transcription of "http://algs4.cs.princeton.edu".
-- (c) 2014 Jeffrey Rosenbluth
----------------------------------------------------------

module Sorting.Heap where

import           Common.References

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Primitive.MutVar
import           Data.Vector.Generic         (Vector)
import           Data.Vector.Generic.Mutable (MVector, unsafeRead, swap, length)
import           Prelude                     hiding (read, length)
import           Sorting.Sorting


sink :: (PrimMonad m, MVector v a)
     => (a -> a -> Ordering) -> v (PrimState m) a -> Int -> Int -> m ()
sink cmp vec l h = go l h
  where
    go k n =
      when (2 * k <= n) $ do
        let j = 2 * k
        a <- unsafeRead vec j
        b <- unsafeRead vec (j + 1)
        let j' = j + if j < n && a `cmp` b == LT then 1 else 0
        c <- unsafeRead vec k
        d <- unsafeRead vec j'
        when (c `cmp` d == LT) $ do
          swap vec k j'
          go j' n

sortBy' :: (PrimMonad m, MVector v a)
     => (a -> a -> Ordering) -> v (PrimState m) a -> m ()
sortBy' cmp vec = do
  let n = length vec - 1
  let m = n `div` 2
  forM_ [m, m-1..0]  $ \k -> sink cmp vec k n
  go n
    where
      go q = when (q > 0) $ do
        let q1 = q - 1
        swap vec 0 q
        sink cmp vec 0 q1
        go q1

sort' :: (PrimMonad m, Ord a, MVector v a) => v (PrimState m) a -> m ()
sort' = sortBy' compare

sortBy :: (Vector v a) => (a -> a -> Ordering) -> v a -> v a
sortBy = toImmutable sortBy'

sort :: (Ord a, Vector v a) => v a -> v a
sort = sortBy compare

sortOn :: (Ord b, Vector v a, Vector v (b, a), Functor v)
       => (a -> b) -> v a -> v a
sortOn = mkSortOn sortBy'
