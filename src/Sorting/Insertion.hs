{-# LANGUAGE FlexibleInstances #-}

----------------------------------------------------------
-- |
-- 2 Sorting
-- Inserting sort.
-- (c) 2014-16 Jeffrey Rosenbluth
----------------------------------------------------------

module Sorting.Insertion where

import           Control.Monad
import           Control.Monad.ST
import           Data.Vector.Generic         (Vector)
import           Data.Vector.Generic.Mutable (MVector, length, unsafeRead,
                                              unsafeSwap)
import           Prelude                     hiding (length)
import           Sorting.Sorting

instance Sortable a where
  sortBy' cmp vec =
    forM_ [0 .. length vec - 1] $ \i ->
      forM_ [i, i - 1 .. 1] $ \j -> do
        vj0 <- unsafeRead vec (j - 1)
        vj1 <- unsafeRead vec j
        when (vj1 `cmp` vj0 == LT) (unsafeSwap vec j (j - 1))
