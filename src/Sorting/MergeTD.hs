{-# LANGUAGE FlexibleContexts  #-}
----------------------------------------------------------
-- |
-- Top down merge sort.
-- (c) 2014-16 Jeffrey Rosenbluth
----------------------------------------------------------
module Sorting.MergeTD
  ( sortBy'
  , sortBy
  , sort'
  , sort
  , sortOn
  ) where

import           Common.References
import           Control.Monad               (when)
import           Control.Monad.Primitive
import           Data.Vector.Generic         (Vector)
import           Data.Vector.Generic.Mutable (MVector, clone, length,
                                              unsafeRead, unsafeWrite)
import           Prelude                     hiding (length)
import           Sorting.Sorting


sortBy' :: (PrimMonad m, MVector v a)
        => Comparing a -> v (PrimState m) a -> m ()
sortBy' cmp vec = do
  let mSort l h = when (l < h) $ do
        let m = l + (h - l) `div` 2
        mSort l m           -- Sort left half.
        mSort (m + 1) h     -- Sort right half.
        merge cmp vec l m h    -- Merge results.
  mSort 0 $ length vec - 1

sort' :: (PrimMonad m, Ord a, MVector v a) => v (PrimState m) a -> m ()
sort' = sortBy' compare

sortBy :: (Vector v a) => Comparing a -> v a -> v a
sortBy = toImmutable sortBy'

sort :: (Ord a, Vector v a) => v a -> v a
sort = sortBy compare

sortOn :: (Ord b, Vector v a, Vector v (b, a), Functor v) => (a -> b) -> v a -> v a
sortOn = mkSortOn sortBy'
