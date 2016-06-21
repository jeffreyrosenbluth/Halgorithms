{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

module Sorting.Sorting where

import           Common.References

import           Control.Monad               (forM_)
import           Control.Monad.ST            (ST, runST)
import           Data.STRef
import           Data.Vector.Generic         (Vector, unsafeThaw, unsafeFreeze)
import           Data.Vector.Generic.Mutable (MVector, unsafeRead, unsafeWrite, clone)

type Sorter = forall v s a. (Ord a, MVector v a) => v s a -> ST s ()

immutableSort :: (Ord a, Vector v a) => Sorter -> v a -> v a
immutableSort sort arr = runST $ do
  vec <- unsafeThaw arr
  sort vec
  unsafeFreeze vec

-- | For merge sort and bottom up merge sort, Algorithms 2.4.
--   Merge 'arr[lo..mid]' with 'arr[mid+1..hi].
merge :: (Ord a, MVector v a) => v s a -> Int -> Int -> Int -> ST s ()
merge vec lo mid hi = do
    r1  <- newSTRef lo
    r2  <- newSTRef (mid + 1)
    aux <- clone vec
    let put i j = unsafeRead aux j >>= unsafeWrite vec i
    forM_ [lo..hi] $ \k -> do
      n1   <- readSTRef r1
      n2   <- readSTRef r2
      aux1 <- unsafeRead aux (min n1 hi)
      aux2 <- unsafeRead aux (min n2 hi)
      if | n1 > mid    -> put k n2 >> r2 += 1
         | n2 > hi     -> put k n1 >> r1 += 1
         | aux2 < aux1 -> put k n2 >> r2 += 1
         | otherwise   -> put k n1 >> r1 += 1
