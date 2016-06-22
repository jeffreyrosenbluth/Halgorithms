{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}

----------------------------------------------------------
-- |
-- 2 Sorting
-- Shellsort.
-- Transcription of "http://algs4.cs.princeton.edu".
-- (c) 2014-16 Jeffrey Rosenbluth
----------------------------------------------------------

module Sorting.Shell where

import           Common.References
import           Control.Monad
import           Control.Monad.ST
import           Data.STRef
import           Data.Vector.Generic         (Vector)
import           Data.Vector.Generic.Mutable (MVector, length, unsafeRead,
                                              unsafeSwap)
import           Prelude                     hiding (length)
import           Sorting.Sorting


-- | Shellsort, Algorithm 2.3. For mutable vectors.
instance Sortable a where
  sortBy' cmp vec = do
    let n      = length vec - 1
        hs     = reverse . takeWhile (<= n `div` 3 + 1)
                         $ iterate (\x -> 3 * x + 1) 1
        go h j = when (j >= h) $ do
          b <- unsafeRead vec j
          a <- unsafeRead vec (j - h)
          when (b `cmp` a == LT) $ do
            unsafeSwap vec j (j - h)
            go h (j - h)
    forM_ hs $ \h' -> forM_ [h' .. n] (go h')
