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
import           Sorting.Sorting

import           Control.Monad               (when)
import           Control.Monad.ST            (ST, runST)
import           Data.STRef
import           Data.Vector.Generic         (Vector)
import           Data.Vector.Generic.Mutable (MVector, unsafeRead, unsafeSwap, length)
import           Prelude                     hiding (length)

-- | Partition into 'vec[lo..i-1], a[+1..hi]'. For quicksort, Algorithm 2.5.
partition :: (Ord a, MVector v a) => v s a -> Int -> Int -> ST s Int
partition vec lo hi = do
  iRef <- newSTRef lo
  jRef <- newSTRef (hi + 1)
  v    <- unsafeRead vec lo
  let setI = do
        iRef += 1
        i'    <- readSTRef iRef
        itemI <- unsafeRead vec i'
        when (itemI < v && i' < hi) setI
      setJ = do
        jRef -= 1
        j'    <- readSTRef jRef
        itemJ <- unsafeRead vec j'
        when (v < itemJ && j' > lo) setJ
      go = do
        setI; setJ
        i <- readSTRef iRef
        j <- readSTRef jRef
        if i < j then unsafeSwap vec i j >> go else return j
  go >>= unsafeSwap vec lo
  readSTRef jRef

-- | Quicksort, Algorithm 2.5. For mutable vectors.
sort' :: (Ord a, MVector v a) => v s a -> ST s ()
sort' vec =   qSort 0 $ length vec - 1
  where
    qSort l h = when (l < h) $ do
      j <- partition vec l h
      qSort l (j - 1)
      qSort (j + 1) h

-- | Quicksort, Algorithm 2.5. For immutable vectors.
sort :: (Ord a, Vector v a) => v a -> v a
sort = immutableSort sort'
