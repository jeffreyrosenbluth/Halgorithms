----------------------------------------------------------
-- |
-- 2 Sorting
-- Selection sort.
-- Transcription of "http://algs4.cs.princeton.edu".
-- (c) 2014-16 Jeffrey Rosenbluth
----------------------------------------------------------

module Sorting.Selection where

import           Common.References
import           Sorting.Sorting

import           Control.Monad
import           Control.Monad.ST
import           Data.STRef
import           Data.Vector.Generic         (Vector)
import           Data.Vector.Generic.Mutable (MVector, unsafeRead, unsafeSwap, length)
import           Prelude                     hiding (length)

-- | Selection sort, Algorithm 2.1. For mutable vectors.
sort' :: (Ord a, MVector v a) => v s a -> ST s ()
sort' vec = do
  let n = length vec
  infRef <- newSTRef 0
  forM_ [0 .. n - 1] $ \i -> do
    infRef .= i
    forM_ [i + 1 .. n - 1] $ \j -> do
      itemJ   <- unsafeRead vec j
      itemInf <- unsafeRead vec =<< readSTRef infRef
      when (itemJ < itemInf) $ infRef .= j
    newInf <- readSTRef infRef
    unsafeSwap vec i newInf

-- | Selection sort, Algorithm 2.1. For immutable vectors.
sort :: (Ord a, Vector v a) => v a -> v a
sort = immutableSort sort'