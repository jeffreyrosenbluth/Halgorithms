{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
----------------------------------------------------------
-- |
-- 2 Sorting
-- Selection sort.
-- Transcription of "http://algs4.cs.princeton.edu".
-- (c) 2014-16 Jeffrey Rosenbluth
----------------------------------------------------------

module Sorting.Selection where

import           Common.References
import           Control.Monad
import           Control.Monad.Primitive
import           Data.Primitive.MutVar
import           Data.Vector.Generic         (Vector)
import           Data.Vector.Generic.Mutable (MVector, length, unsafeRead,
                                              unsafeSwap)
import           Prelude                     hiding (length)
import           Sorting.Sorting

-- | Selection sort, Algorithm 2.1. For mutable vectors.
sortBy' :: (PrimMonad m, MVector v a)
        => (a -> a -> Ordering) -> v (PrimState m) a -> m ()
sortBy' cmp vec = do
  let n = length vec
  infRef <- newMutVar 0
  forM_ [0 .. n - 1] $ \i -> do
    infRef .= i
    forM_ [i + 1 .. n - 1] $ \j -> do
      itemJ   <- unsafeRead vec j
      itemInf <- unsafeRead vec =<< readMutVar infRef
      when (itemJ `cmp` itemInf == LT) $ infRef .= j
    newInf <- readMutVar infRef
    unsafeSwap vec i newInf

sort' :: (PrimMonad m, Ord a, MVector v a) => v (PrimState m) a -> m ()
sort' = sortBy' compare

sortBy :: (Vector v a) => (a -> a -> Ordering) -> v a -> v a
sortBy = toImmutable sortBy'

sort :: (Ord a, Vector v a) => v a -> v a
sort = sortBy compare

sortOn :: (Ord b, Vector v a, Vector v (b, a), Functor v) => (a -> b) -> v a -> v a
sortOn = mkSortOn sortBy'
