{-# LANGUAGE FlexibleContexts  #-}
----------------------------------------------------------
-- |
-- Inserting sort.
-- (c) 2014-16 Jeffrey Rosenbluth
----------------------------------------------------------
module Sorting.Insertion
  ( sortBy'
  , sortBy
  , sort'
  , sort
  , sortOn
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Vector.Generic         (Vector)
import           Data.Vector.Generic.Mutable (MVector, length, unsafeRead,
                                              unsafeSwap)
import           Prelude                     hiding (length)
import           Sorting.Sorting

-- | Insertion sort for mutable vectors
sortBy' :: (PrimMonad m, MVector v a)
        => Comparing a-> v (PrimState m) a -> m ()
sortBy' cmp vec =
  forM_ [0 .. length vec - 1] $ \i ->
    forM_ [i, i - 1 .. 1] $ \j -> do
      vj0 <- unsafeRead vec (j - 1)
      vj1 <- unsafeRead vec j
      when (vj1 `cmp` vj0 == LT) (unsafeSwap vec j (j - 1))

-- | Insertion  sort for mutable vectors.
sort' :: (PrimMonad m, Ord a, MVector v a) => v (PrimState m) a -> m ()
sort' = sortBy' compare

sortBy :: (Vector v a) => Comparing a-> v a -> v a
sortBy = toImmutable sortBy'

sort :: (Ord a, Vector v a) => v a -> v a
sort = sortBy compare

sortOn :: (Ord b, Vector v a, Vector v (b, a), Functor v) => (a -> b) -> v a -> v a
sortOn = mkSortOn sortBy'
