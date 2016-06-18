{-# LANGUAGE MultiWayIf       #-}

----------------------------------------------------------
-- |
-- 2 Sorting
-- Shellsort.
-- Transcription of "http://algs4.cs.princeton.edu".
-- (c) 2014-16 Jeffrey Rosenbluth
----------------------------------------------------------

module Sorting.Shell where

import           Common.References
import           Sorting.Sorting

import           Control.Monad
import           Control.Monad.ST
import           Data.STRef
import           Data.Vector.Generic         (Vector)
import           Data.Vector.Generic.Mutable (MVector, unsafeRead, unsafeSwap, length)
import           Prelude                     hiding (length)


-- | Shellsort, Algorithm 2.3. For mutable vectors.
sort' :: (Ord a, MVector v a) => v s a -> ST s ()
sort' vec = do
  let n      = length vec - 1
      hs     = reverse . takeWhile (<= n `div` 3 + 1)
                       $ iterate (\x -> 3 * x + 1) 1
      go h j = when (j >= h) $ do
        b <- unsafeRead vec j
        a <- unsafeRead vec (j - h)
        when (b < a) $ do
          unsafeSwap vec j (j - h)
          go h (j - h)
  forM_ hs $ \h' -> forM_ [h' .. n] (go h')

-- | Shellsort, Algorithm 2.3. For immutable vectors.
sort :: (Ord a, Vector v a) => v a -> v a
sort = immutableSort sort'
