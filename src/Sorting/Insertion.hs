{-# LANGUAGE MultiWayIf       #-}

----------------------------------------------------------
-- |
-- 2 Sorting
-- Inserting sort.
-- Transcription of "http://algs4.cs.princeton.edu".
-- (c) 2014-16 Jeffrey Rosenbluth
----------------------------------------------------------

module Sorting.Insertion where

import           Control.Monad
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Control.Monad.ST
import           Data.Vector.Generic         (Vector, unsafeThaw, unsafeFreeze)
import           Data.Vector.Generic.Mutable (MVector, unsafeRead, unsafeSwap, length)
import           Prelude                     hiding (length)

-- | Insertion sort, Algorithm 2.2. For mutable vectors.
sort' :: (Ord a, PrimMonad m, MVector v a) => v (PrimState m) a -> m ()
sort' vec =
  forM_ [0 .. length vec - 1]
    (\i -> forM_ [i, i - 1 .. 1]
      (\j -> do
         vj0 <- unsafeRead vec (j - 1)
         vj1 <- unsafeRead vec j
         when (vj1 < vj0) (unsafeSwap vec j (j - 1))))

-- | Insertion sort, Algorithm 2.2. For immutable vectors.
sort :: (Ord a, Vector v a) => v a -> v a
sort arr = runST $ do
  vec <- unsafeThaw arr
  sort' vec
  unsafeFreeze vec
