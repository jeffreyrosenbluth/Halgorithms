{-# LANGUAGE FlexibleInstances #-}

----------------------------------------------------------
-- |
-- 2 Sorting
-- Selection sort.
-- Transcription of "http://algs4.cs.princeton.edu".
-- (c) 2014-16 Jeffrey Rosenbluth
----------------------------------------------------------

module Sorting.Selection where

import           Common.References
import           Control.Monad
import           Control.Monad.ST
import           Data.STRef
import           Data.Vector.Generic         (Vector)
import           Data.Vector.Generic.Mutable (MVector, length, unsafeRead,
                                              unsafeSwap)
import           Prelude                     hiding (length)
import           Sorting.Sorting

-- | Selection sort, Algorithm 2.1. For mutable vectors.
instance Sortable a where
  sortBy' cmp vec = do
    let n = length vec
    infRef <- newSTRef 0
    forM_ [0 .. n - 1] $ \i -> do
      infRef .= i
      forM_ [i + 1 .. n - 1] $ \j -> do
        itemJ   <- unsafeRead vec j
        itemInf <- unsafeRead vec =<< readSTRef infRef
        when (itemJ `cmp` itemInf == LT) $ infRef .= j
      newInf <- readSTRef infRef
      unsafeSwap vec i newInf
