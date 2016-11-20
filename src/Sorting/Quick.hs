{-# LANGUAGE FlexibleContexts  #-}
----------------------------------------------------------
-- |
-- Quicksort (in place).
-- (c) 2014-16 Jeffrey Rosenbluth
----------------------------------------------------------s
module Sorting.Quick
  ( sortBy'
  , sortBy
  , sort'
  , sort
  , sortOn
  ) where

import           Common.References
import           Sorting.Sorting
import           Control.Monad               (when)
import           Control.Monad.Primitive
import           Data.Primitive.MutVar
import           Data.Vector.Generic         (Vector)
import           Data.Vector.Generic.Mutable (MVector, unsafeRead, unsafeSwap, length)
import           Prelude                     hiding (length)

-- | Partition into 'vec[lo..i-1], a[+1..hi]'.
partition :: (PrimMonad m, MVector v a )
          => Comparing a -> v (PrimState m) a -> Int -> Int -> m Int
partition cmp vec lo hi = do
  iRef <- newMutVar lo
  jRef <- newMutVar (hi + 1)
  v    <- unsafeRead vec lo
  let left = do
        iRef += 1
        i     <- readMutVar iRef
        itemI <- unsafeRead vec i
        when (itemI `cmp` v == LT && i < hi) left
      right = do
        jRef -= 1
        j     <- readMutVar jRef
        itemJ <- unsafeRead vec j
        when (itemJ `cmp` v == GT && j > lo) right
      go = do
        left
        i <- readMutVar iRef
        right
        j <- readMutVar jRef
        if i < j then unsafeSwap vec i j >> go else return j
  go >>= unsafeSwap vec lo
  readMutVar jRef

-- | Quicksort, Algorithm 2.5. For mutable vectors.
sortBy' :: (PrimMonad m, MVector v a)
        => Comparing a -> v (PrimState m) a -> m ()
sortBy' cmp vec =   qSort 0 $ length vec - 1
  where
    qSort l h = when (l < h) $ do
      j <- partition cmp vec l h
      qSort l (j - 1)
      qSort (j + 1) h

sort' :: (PrimMonad m, Ord a, MVector v a) => v (PrimState m) a -> m ()
sort' = sortBy' compare

sortBy :: (Vector v a) => Comparing a -> v a -> v a
sortBy = toImmutable sortBy'

sort :: (Ord a, Vector v a) => v a -> v a
sort = sortBy compare

sortOn :: (Ord b, Vector v a, Vector v (b, a), Functor v) => (a -> b) -> v a -> v a
sortOn = mkSortOn sortBy'
