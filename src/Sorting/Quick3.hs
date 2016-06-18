{-# LANGUAGE MultiWayIf       #-}

----------------------------------------------------------
-- |
-- 2 Sorting
-- Selection sort, Inserting sort, Shellsort, Megesort,
-- Bottum up mergesort and QuickSort.
-- Transcription of "http://algs4.cs.princeton.edu".
-- (c) 2014 Jeffrey Rosenbluth
----------------------------------------------------------

module Sorting.Quick3 where

import           Common.References

import           Control.Monad
import           Control.Monad.ST
import           Data.STRef
import           Data.Vector.Generic         (Vector, unsafeThaw, unsafeFreeze)
import           Data.Vector.Generic.Mutable (read, write, swap, MVector)
import qualified Data.Vector.Generic.Mutable as VM
import           Prelude                     hiding (read)

-- | Quicksort with 3-way partitioning
-- sort :: (Ord a, Vector v a) => v a -> v a
-- sort arr = runST $ do
--   vec <- unsafeThaw arr
--   let hi = VM.length vec - 1
--       sort l h = when (l < h) $ do
--         ltRef <- newSTRef l
--         iRef  <- newSTRef (l + 1)
--         gtRef <- newSTRef h
--         v <- read vec l
--         v <- read vec l
--       let go = do
--             i     <- readSTRef iRef
--             lt    <- readSTRef ltRef
--             gt    <- readSTRef gtRef
--             itemI <- read vec (min i h)
--             when (i <= gt) $ do
--               if | itemI < v -> do
--                      swap vec lt i
--                      ltRef += 1
--                      iRef += 1
--                  | itemI > v -> do
--                      swap vec i gt
--                      gtRef -= 1
--                  | otherwise -> iRef += 1
--               go
--       go
--       lt <- readSTRef ltRef
--       gt <- readSTRef gtRef
--       -- Now 'arr[l..lt-1] < v = arr[lt..gt] < arr[gt+1..h]'.
--       sort l (lt - 1)
--       sort (gt + 1) h
--   sort 0 hi
--   unsafeFreeze vec
