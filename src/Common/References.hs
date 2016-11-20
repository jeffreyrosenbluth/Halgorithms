module Common.References where

import           Control.Monad.Primitive
import           Data.Primitive.MutVar

-- | Helper function assigning a value to an 'MutVar'
(.=) :: (PrimMonad m) => MutVar (PrimState m) a -> a -> m ()
(.=) = writeMutVar
infix  4 .=

-- | Helper function adding a value to an 'MutVar'
(+=) :: (PrimMonad m, Num a) => MutVar (PrimState m) a -> a -> m ()
(+=) v n = modifyMutVar' v (+n)
infix  4 +=

-- | Helper function subtracting a value to an 'MutVar'
(-=) :: (PrimMonad m, Num a) => MutVar (PrimState m) a -> a -> m ()
(-=) v n = modifyMutVar' v (\x -> x - n)
infix  4 -=
