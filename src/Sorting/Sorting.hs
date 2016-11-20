{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}

module Sorting.Sorting where

import           Common.References
import           Control.Monad               (forM_)
import           Control.Monad.ST            (ST, runST)
import           Control.Monad.Primitive
import           Data.Ord                    (comparing)
import           Data.Primitive.MutVar
import           Data.STRef
import           Data.Vector.Generic         (Vector, unsafeThaw, unsafeFreeze)
import           Data.Vector.Generic.Mutable (MVector, unsafeRead, unsafeWrite, clone)

type Sorter = forall v s a. MVector v a => (a -> a -> Ordering) -> v s a -> ST s ()

toImmutable :: (Vector v a) => Sorter -> (a -> a -> Ordering) -> v a -> v a
toImmutable sorter cmp arr = runST $ do
  vec <- unsafeThaw arr
  sorter cmp vec
  unsafeFreeze vec

mkSortOn :: (Ord b, Vector v a, Vector v (b, a), Functor v)
     => Sorter -> (a -> b) -> v a -> v a
mkSortOn sorter f = fmap snd
       . toImmutable sorter (comparing fst)
       . fmap (\x -> let y = f x in y `seq` (y, x))

-- | For merge sort and bottom up merge sort, Algorithms 2.4.
--   Merge 'arr[lo..mid]' with 'arr[mid+1..hi].
merge :: (PrimMonad m, MVector v a)
      => (a -> a -> Ordering) -> v (PrimState m) a -> Int -> Int -> Int -> m ()
merge cmp vec lo mid hi = do
    r1  <- newMutVar lo
    r2  <- newMutVar (mid + 1)
    aux <- clone vec
    let put i j = unsafeRead aux j >>= unsafeWrite vec i
    forM_ [lo..hi] $ \k -> do
      n1   <- readMutVar r1
      n2   <- readMutVar r2
      aux1 <- unsafeRead aux (min n1 hi)
      aux2 <- unsafeRead aux (min n2 hi)
      if | n1 > mid -> put k n2 >> r2 += 1
         | n2 > hi  -> put k n1 >> r1 += 1
         | aux2 `cmp` aux1 == LT -> put k n2 >> r2 += 1
         | otherwise -> put k n1 >> r1 += 1
