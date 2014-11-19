{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}

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

selection :: (MArray a e (ST s), Ix i, Ord e, Num i, Enum i) 
          => a i e -> ST s ()
selection arr = do
  (m, n) <- getBounds arr
  infRef <- newSTRef 0
  forM_ [m..n] $ \i -> do
    writeSTRef infRef i
    forM_ [i+1..n] $ \j -> do
      inf     <- readSTRef infRef
      itemJ   <- readArray arr j
      itemInf <- readArray arr inf
      when (itemJ < itemInf) $ writeSTRef infRef j
    newInf <- readSTRef infRef
    exch arr i newInf

insertion :: (MArray a e m, Ix i, Ord e, Num i, Enum i) 
          => a i e -> m ()
insertion arr = do
  (m, n) <- getBounds arr
  forM_ [m+1..n] go
    where
      go 0 = return ()
      go n = do
        b <- readArray arr n
        a <- readArray arr (n-1)
        when (b < a) $ do
          exch arr n (n-1)
          go (n-1)

shell :: (MArray a e (ST s), Ix i, Ord e, Integral i) 
      => a i e -> ST s ()
shell arr = do
  (_, n) <- getBounds arr
  let hs = reverse $ takeWhile (<= n `div` 3 + 1) (iterate (\x -> 3 * x + 1) 1)
  forM_ hs $ \h' ->
    forM_ [h'..n] (go h')
    where
      go h j = 
        when (j >= h) $ do
          b <- readArray arr j
          a <- readArray arr (j - h)
          when (b < a) $ do
            exch arr j (j - h)
            go h (j - h)

mergeAux :: (MArray a e (ST s), Ix i, Ord e, Num i, Enum i)
             => a i e -> i -> i -> i -> ST s ()
mergeAux arr lo mid hi = do
    iRef <- newSTRef lo
    jRef <- newSTRef (mid + 1)
    aux  <- mapArray id arr 
    forM_ [lo..hi] $ \k -> do
      i    <- readSTRef iRef
      j    <- readSTRef jRef
      auxi <- readArray aux (min i hi)
      auxj <- readArray aux (min j hi)
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
             
merge :: (MArray a e (ST s), Ix i, Ord e, Integral i) => a i e -> ST s () 
merge arr = do
  (lo, hi) <- getBounds arr
  sort lo hi
    where
      sort l h = when (l < h) $ do
        let m = l + (h - l) `div` 2
        sort l m
        sort (m + 1) h
        mergeAux arr l m h

mergeBU :: (MArray a e (ST s), Ix i, Ord e, Num i, Enum i) => a i e -> ST s ()
mergeBU arr = do
  (_, hi) <- getBounds arr
  forM_ (takeWhile (< hi) [2 ^ i | i <- [0..]]) $ \sz ->
    forM_ [0, sz + sz .. hi - sz - 1] $ \i ->
      mergeAux arr i (i + sz - 1) (min (i + sz + sz - 1) (hi - 1))

main :: IO ()
main = print $ runST $ do
  xs <- newListArray (0, 15) "MERGESORTEXAMPLE" :: (ST s (STUArray s Int Char))
  _  <- mergeBU xs
  getElems xs
