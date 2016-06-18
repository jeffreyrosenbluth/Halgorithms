----------------------------------------------------------
-- |
-- 2 Sorting
-- Selection sort, Inserting sort, Shellsort, Megesort,
-- Bottum up mergesort and QuickSort.
-- Transcription of "http://algs4.cs.princeton.edu".
-- (c) 2014 Jeffrey Rosenbluth
----------------------------------------------------------

module Sorting.MergeTD where

import           Common.References
import           Sorting.Sorting

import           Control.Monad               (when)
import           Control.Monad.ST            (ST)
import           Data.Vector.Generic         (Vector)
import           Data.Vector.Generic.Mutable (MVector, unsafeRead, unsafeWrite, length, clone)
import           Prelude                     hiding (length)


-- | Top-down mergesort, Algorithm 2.4. For mutable vectors.
sort' :: (Ord a, MVector v a) => v s a -> ST s ()
sort' vec = do
  let mSort l h = when (l < h) $ do
        let m = l + (h - l) `div` 2
        mSort l m           -- Sort left half.
        mSort (m + 1) h     -- Sort right half.
        merge vec l m h    -- Merge results.
  mSort 0 $ length vec - 1

-- | Top-down mergesort, Algorithm 2.4. For immutable vectors
sort :: (Ord a, Vector v a) => v a -> v a
sort = immutableSort sort'
