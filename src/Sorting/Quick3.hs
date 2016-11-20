{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}
----------------------------------------------------------
-- |
-- 2 Sorting
-- Selection sort, Inserting sort, Shellsort, Megesort,
-- Bottum up mergesort and QuickSort.
-- Transcription of "http://algs4.cs.princeton.edu".
-- (c) 2014 Jeffrey Rosenbluth
----------------------------------------------------------

module Sorting.Quick3 where

import           Common.References
import           Control.Monad               (when)
import           Control.Monad.Primitive
import           Data.Primitive.MutVar
import           Data.Vector.Generic         (Vector)
import           Data.Vector.Generic.Mutable (MVector, length, unsafeRead,
                                              unsafeSwap)
import           Prelude                     hiding (length)
import           Sorting.Sorting

-- | Quicksort with 3-way partitioning
sortBy' :: (PrimMonad m, MVector v a)
        => (a -> a -> Ordering) -> v (PrimState m) a -> m ()
sortBy' cmp vec = do
  let hi = length vec - 1
  qSort 0 hi
    where
      qSort l h = when (l < h) $ do
        ltRef <- newMutVar l
        iRef  <- newMutVar (l + 1)
        gtRef <- newMutVar h
        v <- unsafeRead vec l

        let go = do
              i     <- readMutVar iRef
              lt    <- readMutVar ltRef
              gt    <- readMutVar gtRef
              itemI <- unsafeRead vec (min i h)
              when (i <= gt) $ do
                if | itemI `cmp` v == LT -> unsafeSwap vec lt i >> ltRef += 1 >> iRef += 1
                   | itemI `cmp` v == GT -> unsafeSwap vec i gt >> gtRef -= 1
                   | otherwise -> iRef += 1
                go

        go
        lt <- readMutVar ltRef
        gt <- readMutVar gtRef
        qSort l (lt - 1)
        qSort (gt + 1) h

sort' :: (PrimMonad m, Ord a, MVector v a) => v (PrimState m) a -> m ()
sort' = sortBy' compare

sortBy :: (Vector v a) => (a -> a -> Ordering) -> v a -> v a
sortBy = toImmutable sortBy'

sort :: (Ord a, Vector v a) => v a -> v a
sort = sortBy compare

sortOn :: (Ord b, Vector v a, Vector v (b, a), Functor v) => (a -> b) -> v a -> v a
sortOn = mkSortOn sortBy'
