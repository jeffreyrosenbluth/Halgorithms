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
import           Control.Monad.ST
import           Data.STRef
import           Data.Vector.Generic         (Vector)
import           Data.Vector.Generic.Mutable (MVector, length, unsafeRead,
                                              unsafeSwap)
import           Prelude                     hiding (length)
import           Sorting.Sorting

-- | Quicksort with 3-way partitioning
instance Sortable a where
  sortBy' cmp vec = do
      let hi = length vec - 1
      qSort 0 hi
        where
          qSort l h = when (l < h) $ do
            ltRef <- newSTRef l
            iRef  <- newSTRef (l + 1)
            gtRef <- newSTRef h
            v <- unsafeRead vec l
            v <- unsafeRead vec l

            let go = do
                  i     <- readSTRef iRef
                  lt    <- readSTRef ltRef
                  gt    <- readSTRef gtRef
                  itemI <- unsafeRead vec (min i h)
                  when (i <= gt) $ do
                    if | itemI `cmp` v == LT -> do
                           unsafeSwap vec lt i
                           ltRef += 1
                           iRef += 1
                       | itemI `cmp` v == GT -> do
                           unsafeSwap vec i gt
                           gtRef -= 1
                       | otherwise -> iRef += 1
                    go

            go
            lt <- readSTRef ltRef
            gt <- readSTRef gtRef
            -- Now 'arr[l..lt-1] < v = arr[lt..gt] < arr[gt+1..h]'.
            qSort l (lt - 1)
            qSort (gt + 1) h
