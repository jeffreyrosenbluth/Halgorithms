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
    iRef <- newSTRef lo
    jRef <- newSTRef (mid + 1)
    -- Allocate space just once and copy the array.
    aux  <- clone vec
    forM_ [lo..hi] $ \k -> do
      i    <- readSTRef iRef
      j    <- readSTRef jRef
      auxi <- unsafeRead aux (min i hi)
      auxj <- unsafeRead aux (min j hi)
      -- Merge back to 'vec[lo..hi]
      if | i > mid -> do
             unsafeWrite vec k =<< unsafeRead aux j
             jRef .= j + 1
         | j > hi -> do
             unsafeWrite vec k =<< unsafeRead aux i
             iRef .= i + 1
         | auxj < auxi -> do
             unsafeWrite vec k =<< unsafeRead aux j
             jRef .= j + 1
         | otherwise -> do
             unsafeWrite vec k =<< unsafeRead aux i
             iRef .= i + 1
