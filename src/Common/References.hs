module Common.References where

import           Control.Monad.ST
import           Data.STRef

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
