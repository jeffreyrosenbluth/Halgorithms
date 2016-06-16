{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}

----------------------------------------------------------
-- |
-- 2 Sorting
-- Selection sort, Inserting sort, Shellsort, Megesort,
-- Bottum up mergesort and QuickSort.
-- Transcription of "http://algs4.cs.princeton.edu".
-- (c) 2014 Jeffrey Rosenbluth
----------------------------------------------------------

module Sorting where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.STRef
import qualified Data.Vector.Unboxed         as V
import           Data.Vector.Generic         (Vector, unsafeThaw, unsafeFreeze, toList, fromList)
import           Data.Vector.Generic.Mutable (read, write, swap, MVector)
import qualified Data.Vector.Generic.Mutable as VM
import           Prelude                     hiding (read)

-- | Helper function assigning a value to an 'STRef'
(.=) :: STRef s a -> a -> ST s ()
(.=) = writeSTRef
infix  4 .=

-- | Helper function adding a value to an 'STRef'
(+=) :: Num a => STRef s a -> a -> ST s ()
(+=) v n = modifySTRef' v (+n)
infix  4 +=

-- | Helper function subtracting a value to an 'STRef'
(-=) :: Num a => STRef s a -> a -> ST s ()
(-=) v n = modifySTRef' v (\x -> x - n)
infix  4 -=

-- | Selection sort, Algorithm 2.1
selectionSort :: (Ord a, Vector v a) => v a -> v a
selectionSort arr = runST $ do
  vec <- unsafeThaw arr
  let n = VM.length vec
  infRef <- newSTRef 0
  forM_ [0..n-1] $ \i -> do
    infRef .= i
    forM_ [i+1..n-1] $ \j -> do
      itemJ   <- read vec j
      itemInf <- read vec =<< readSTRef infRef
      when (itemJ < itemInf) $ infRef .= j
    newInf <- readSTRef infRef
    swap vec i newInf
  unsafeFreeze vec

-- | Insertion sort, Algorithm 2.2
insertionSort :: (Ord a, Vector v a) => v a -> v a
insertionSort arr = runST $ do
  vec <- unsafeThaw arr
  let m    = VM.length vec
      go n = when (n > 0) $ do
        b <- read vec n
        a <- read vec (n - 1)
        when (b < a) $ do
          swap vec n (n - 1)
          go (n - 1)
  forM_ [1..m-1] go
  unsafeFreeze vec

-- | Shellsort, Algorithm 2.3
shellSort :: (Ord a, Vector v a) => v a -> v a
shellSort arr = runST $ do
  vec <- unsafeThaw arr
  let n      = VM.length vec - 1
      hs     = reverse . takeWhile (<= n `div` 3 + 1) $ iterate (\x -> 3 * x + 1) 1
      go h j = when (j >= h) $ do
        b <- read vec j
        a <- read vec (j - h)
        when (b < a) $ do
          swap vec j (j - h)
          go h (j - h)
  forM_ hs $ \h' -> forM_ [h'..n] (go h')
  unsafeFreeze vec

-- | For merge sort and bottom up merge sort, Algorithms 2.4.
--   Merge 'arr[lo..mid]' with 'arr[mid+1..hi].
merge :: (Ord a, MVector v a) => v s a -> Int -> Int -> Int -> ST s ()
merge vec lo mid hi = do
    iRef <- newSTRef lo
    jRef <- newSTRef (mid + 1)
    -- Allocate space just once and copy the array.
    aux  <- VM.clone vec
    forM_ [lo..hi] $ \k -> do
      i    <- readSTRef iRef
      j    <- readSTRef jRef
      auxi <- read aux (min i hi)
      auxj <- read aux (min j hi)
      -- Merge back to 'vec[lo..hi]
      if | i > mid -> do
             write vec k =<< read aux j
             jRef .= j + 1
         | j > hi -> do
             write vec k =<< read aux i
             iRef .= i + 1
         | auxj < auxi -> do
             write vec k =<< read aux j
             jRef .= j + 1
         | otherwise -> do
             write vec k =<< read aux i
             iRef .= i + 1

-- | Top-down mergesort, Algorithm 2.4
mergeSort :: (Ord a, Vector v a) => v a -> v a
mergeSort arr = runST $ do
  vec <- unsafeThaw arr
  let hi = VM.length vec - 1
      sort l h = when (l < h) $ do
        let m = l + (h - l) `div` 2
        sort l m           -- Sort left half.
        sort (m + 1) h     -- Sort right half.
        merge vec l m h -- Merge results.
  sort 0 hi
  unsafeFreeze vec

-- | Bottom-up mergesort.
mergeSortBU :: (Ord a, Vector v a) => v a -> v a
mergeSortBU arr = runST $ do
  vec <- unsafeThaw arr
  let hi = VM.length vec
  -- Do log hi passes of pairwise merges.
  -- sz: subarray size
  -- i:  subarray index
  forM_ (takeWhile (< hi) [2 ^ i | i <- [(0 :: Int)..]]) $ \sz ->
    forM_ [0, sz + sz .. hi - sz - 1] $ \i ->
      merge vec i (i + sz - 1) (min (i + sz + sz - 1) (hi - 1))
  unsafeFreeze vec

-- | Partition into 'vec[lo..i-1], a[+1..hi]'. For quicksort, Algorithm 2.5.
partition :: (Ord a, MVector v a) => v s a -> Int -> Int -> ST s Int
partition vec lo hi = do
  iRef <- newSTRef lo       -- left scan index
  jRef <- newSTRef (hi + 1) -- right scan index
  v    <- read vec lo  -- partioning item
  v    <- read vec lo  -- partioning item
  let setI = do
        modifySTRef' iRef (+1)
        i' <- readSTRef iRef
        itemI <- read vec i'
        itemI <- read vec i'
        when (itemI < v && i' < hi) setI
      setJ = do
        modifySTRef' jRef (+(-1))
        j' <- readSTRef jRef
        itemJ <- read vec j'
        itemJ <- read vec j'
        when (v < itemJ && j' > lo) setJ
      -- Scan rigth, scan left, check for scan complete and swapange.
      go = do
        setI; setJ
        i <- readSTRef iRef
        j <- readSTRef jRef
        if | (i < j) -> do
               swap vec i j
               go
           | otherwise -> return j
  swap vec lo =<< go        -- Put 'v = arr[j]' into position
  readSTRef jRef -- with arr[lo..j-1] <= arr[j+1..hi].

-- | Quicksort, Algorithm 2.5.
quickSort :: (Ord a, Vector v a) => v a -> v a
quickSort arr = runST $ do
  vec <- unsafeThaw arr
  let hi = VM.length vec - 1
      sort l h = when (l < h) $ do
      j <- partition vec l h
      sort l (j-1)  -- Sort left part 'arr[l..j-1]'.
      sort (j+1) h  -- Sort right part 'arr[j+1..h]'.
  sort 0 hi
  unsafeFreeze vec

-- | Quicksort with 3-way partitioning
quickSort3 :: (Ord a, Vector v a) => v a -> v a
quickSort3 arr = runST $ do
  vec <- unsafeThaw arr
  let hi = VM.length vec - 1
      sort l h = when (l < h) $ do
      ltRef <- newSTRef l
      iRef  <- newSTRef (l + 1)
      gtRef <- newSTRef h
      v <- read vec l
      v <- read vec l
      let go = do
            i     <- readSTRef iRef
            lt    <- readSTRef ltRef
            gt    <- readSTRef gtRef
            itemI <- read vec (min i h)
            when (i <= gt) $ do
              if | itemI < v -> do
                     swap vec lt i
                     ltRef += 1
                     iRef += 1
                 | itemI > v -> do
                     swap vec i gt
                     gtRef -= 1
                 | otherwise -> iRef += 1
              go
      go
      lt <- readSTRef ltRef
      gt <- readSTRef gtRef
      -- Now 'arr[l..lt-1] < v = arr[lt..gt] < arr[gt+1..h]'.
      sort l (lt - 1)
      sort (gt + 1) h
  sort 0 hi
  unsafeFreeze vec

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

test :: Vector v a => (v a -> v a) -> [a] -> v a
test f xs = f (fromList xs)

main :: IO ()
main = print $ (test  quickSort3 "MERGESORTEXAMPLE" :: V.Vector Char)
