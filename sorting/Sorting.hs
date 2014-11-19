{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}

----------------------------------------------------------
-- |
-- 2 Sorting
-- Selection sort, Inserting sort, Shellsort, Megesort,
-- Bottum up mergesort and QuickSort.
-- Transcription of "http://algs4.cs.princeton.edu".
-- (c) 2014 Jeffrey Rosenbluth
----------------------------------------------------------
--
module Sorting where

import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Data.STRef

exch :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
exch arr i j = do
  iItem <- readArray arr i
  jItem <- readArray arr j
  writeArray arr i jItem
  writeArray arr j iItem

-- | Selection sort, Algorithm 2.1
--   Sort 'arr' into increasing order.
selection :: (MArray a e (ST s), Ix i, Ord e, Num i, Enum i) 
          => a i e -> ST s ()
selection arr = do
  (m, n) <- getBounds arr
  infRef <- newSTRef 0
  forM_ [m..n] $ \i -> do
    writeSTRef infRef i
    -- Exchange arr[i] with smallest entry in arr[i+1..n].
    forM_ [i+1..n] $ \j -> do
      inf     <- readSTRef infRef -- Index of minimal entry.
      itemJ   <- readArray arr j
      itemInf <- readArray arr inf
      when (itemJ < itemInf) $ writeSTRef infRef j
    newInf <- readSTRef infRef
    exch arr i newInf

-- | Insertion sort, Algorithm 2.2
--   Sort 'arr' into increasing order.
insertion :: (MArray a e m, Ix i, Ord e, Num i, Enum i) 
          => a i e -> m ()
insertion arr = do
  (m, n) <- getBounds arr
  forM_ [m+1..n] go
    -- Insert 'arr[i]' among 'a[i-1], a[i-2], ...
    where
      go 0 = return ()
      go n = do
        b <- readArray arr n
        a <- readArray arr (n-1)
        when (b < a) $ do
          exch arr n (n-1)
          go (n-1)

-- | Shellsort, Algorithm 2.3
--   Sort 'arr' into increasing order.
shell :: (MArray a e (ST s), Ix i, Ord e, Integral i) 
      => a i e -> ST s ()
shell arr = do
  (_, n) <- getBounds arr
  -- 1, 4, 13, 40, 121, 364, 1093, ...
  let hs = reverse $ takeWhile (<= n `div` 3 + 1) (iterate (\x -> 3 * x + 1) 1)
  forM_ hs $ \h' ->
    -- h-sort the array.
    forM_ [h'..n] (go h')
    where
      -- Insert 'a[i]' among 'a[i-h], a[i-2*h], ...
      go h j = 
        when (j >= h) $ do
          b <- readArray arr j
          a <- readArray arr (j - h)
          when (b < a) $ do
            exch arr j (j - h)
            go h (j - h)

-- | For merge sort and bottom up merge sort, Algorithms 2.4a & 2.4b.
--   Merge 'arr[lo..mid]' with 'arr[mid+1..hi].
mergeAux :: (MArray a e (ST s), Ix i, Ord e, Num i, Enum i)
             => a i e -> i -> i -> i -> ST s ()
mergeAux arr lo mid hi = do
    iRef <- newSTRef lo
    jRef <- newSTRef (mid + 1)
    -- Allocate space just once and copy the array.
    aux  <- mapArray id arr 
    forM_ [lo..hi] $ \k -> do
      i    <- readSTRef iRef
      j    <- readSTRef jRef
      auxi <- readArray aux (min i hi)
      auxj <- readArray aux (min j hi)
      -- Merge back to 'arr[lo..hi]
      if | i > mid -> do 
             writeArray arr k =<< readArray aux j
             writeSTRef jRef (j + 1)
         | j > hi -> do 
             writeArray arr k =<< readArray aux i
             writeSTRef iRef (i + 1)
         | auxj < auxi -> do
             writeArray arr k =<< readArray aux j
             writeSTRef jRef (j + 1)
         | otherwise -> do
             writeArray arr k =<< readArray aux i
             writeSTRef iRef (i + 1)
             
-- | Top-down mergesort, Algorithm 2.4a
--   Sort 'arr' into increasing order.
merge :: (MArray a e (ST s), Ix i, Ord e, Integral i) => a i e -> ST s () 
merge arr = do
  (lo, hi) <- getBounds arr
  sort lo hi
    where
      -- Sort arr[lo..hi].
      sort l h = when (l < h) $ do
        let m = l + (h - l) `div` 2
        sort l m           -- Sort left half.
        sort (m + 1) h     -- Sort right half.
        mergeAux arr l m h -- Merge results.

-- | Bottom-up mergesort, Algorithm 2.4b
mergeBU :: (MArray a e (ST s), Ix i, Ord e, Num i, Enum i) => a i e -> ST s ()
mergeBU arr = do
  (_, hi) <- getBounds arr
  -- Do log hi passes of pairwise merges.
  forM_ (takeWhile (< hi) [2 ^ i | i <- [0..]]) $ \sz -> -- sz: subarray size
    forM_ [0, sz + sz .. hi - sz - 1] $ \i ->            -- i:  subarray index
      mergeAux arr i (i + sz - 1) (min (i + sz + sz - 1) (hi - 1)) 

main :: IO ()
main = print $ runST $ do
  xs <- newListArray (0, 15) "MERGESORTEXAMPLE" :: (ST s (STUArray s Int Char))
  _  <- mergeBU xs
  getElems xs
