{-# LANGUAGE FlexibleInstances #-}


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
import           Control.Monad               (when)
import           Control.Monad.ST            (ST)
import           Data.Vector.Generic         (Vector)
import           Data.Vector.Generic.Mutable (MVector, clone, length,
                                              unsafeRead, unsafeWrite)
import           Prelude                     hiding (length)
import           Sorting.Sorting


instance Sortable a where
  sortBy' cmp vec = do
    let mSort l h = when (l < h) $ do
          let m = l + (h - l) `div` 2
          mSort l m           -- Sort left half.
          mSort (m + 1) h     -- Sort right half.
          merge cmp vec l m h    -- Merge results.
    mSort 0 $ length vec - 1
