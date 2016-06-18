{-# LANGUAGE MultiWayIf       #-}

----------------------------------------------------------
-- |
-- 2 Sorting
-- Selection sort, Inserting sort, Shellsort, Megesort,
-- Bottum up mergesort and QuickSort.
-- Transcription of "http://algs4.cs.princeton.edu".
-- (c) 2014 Jeffrey Rosenbluth
----------------------------------------------------------

module Sorting.Heap where

import           Common.References

import           Control.Monad
import           Control.Monad.ST
import           Data.STRef
import           Data.Vector.Generic         (Vector, unsafeThaw, unsafeFreeze)
import           Data.Vector.Generic.Mutable (read, write, swap, MVector)
import qualified Data.Vector.Generic.Mutable as VM
import           Prelude                     hiding (read)

sink :: (Ord a, MVector v a) => v s a -> Int -> Int -> ST s ()
sink = undefined

heap :: (Ord a, MVector v a) => v s a -> ST s ()
heap vec = do
  let n = VM.length vec - 1
  let m = (n - 1) `div` 2
  forM_ [m, m-1..0]  $ \k -> sink vec k n
  go n
    where
      go q = when (q > 0) $ do
        let q1 = q - 1
        swap vec 0 q
        sink vec 0 q1
        go q1
