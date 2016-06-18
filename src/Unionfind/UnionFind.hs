----------------------------------------------------------
-- |
-- 1.5 Case Study: Union-Find
-- Weighted quick-union with path compression.
-- Transcription of "http://algs4.cs.princeton.edu/15uf".
-- (c) 2014 Jeffrey Rosenbluth
----------------------------------------------------------

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Array.ST

data UF s = UF
  { iD :: STUArray s Int Int -- parent link (site indexed)
  , sZ :: STUArray s Int Int -- size of component for roots (site indexed)
  } deriving (Eq)

-- | Returns the component identifier for the component containing
--   the site.
find :: UF s -> Int -> ST s Int
find uf p = do
  q <- readArray (iD uf) p
  if q == p
    then return p
    -- Follow links to find root.
    else do
      r <- find uf q
      -- Path compression.
      writeArray (iD uf) p r
      return r

-- | Merges the component containing site p with the
--   component containing site q.
union :: UF s -> Int -> Int -> ST s ()
union uf p q = do
  i <- find uf p
  j <- find uf q
  unless (i == j) $ do 
  szi <- readArray (sZ uf) i
  szj <- readArray (sZ uf) j
  -- Make smaller root point to larger one.
  if szi < szj 
    then do
      writeArray (iD uf) i j
      writeArray (sZ uf) j (szi + szj)
    else do
      writeArray (iD uf) j i
      writeArray (sZ uf) i (szi + szj)

-- | Are the two sites connected and in the same component?
connected :: UF s -> Int -> Int -> ST s Bool
connected uf p q = (==) <$> find uf p <*> find uf q

-- | Example of tinyUF from:
--   "http://algs4.cs.princeton.edu/15uf/tinyUF.txt"
tinyUF :: Int -> Int -> Bool
tinyUF p q = runST $ do
  ps <- newListArray (0, 9) [0..]
  ss <- newListArray (0, 9) (repeat 0)
  let uf    = UF ps ss
      links = [(4,3), (3,8), (6,5), (9,4), (2,1),(5,0),(7,2),(6,1)]
  mapM_ (uncurry $ union uf) links
  connected uf p q

-- | Check each pair of sites in tinyUF.
main :: IO ()
main = do
  let xs = [(p, q, tinyUF p q) | p <- [0..9], q <- [0..9]] 
  mapM_ print xs
