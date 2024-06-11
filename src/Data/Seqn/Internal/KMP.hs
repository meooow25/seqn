{-# LANGUAGE BangPatterns #-}

module Data.Seqn.Internal.KMP
  ( Table
  , State
  , build
  , step
  ) where

import Data.Primitive.Array (Array, indexArray, sizeofArray)
import Data.Primitive.PrimArray
  ( PrimArray
  , indexPrimArray
  , newPrimArray
  , readPrimArray
  , runPrimArray
  , writePrimArray
  )

-- Knuth–Morris–Pratt algorithm
-- See https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm
--
-- In Table xa pa,
-- * xa is the pattern.
-- * pa is the prefix function. pa!i is the length of longest proper prefix of
--   xa that ends at index i of xa.

data Table a = Table
  {-# UNPACK #-} !(Array a)
  {-# UNPACK #-} !(PrimArray Int)

newtype State a = State Int

-- Precondition: 0 < length xa
build :: Eq a => Array a -> (Table a, State a)
build xa
  | n <= 0 = error "non-positive length"
  | otherwise = (Table xa pa, State 0)
  where
    n = sizeofArray xa
    !pa = runPrimArray $ do
      pma <- newPrimArray n
      writePrimArray pma 0 0
      for_ 1 (n-1) $ \i -> do
        let go j | indexArray xa i == indexArray xa j = pure (j+1)
            go 0 = pure 0
            go j = readPrimArray pma (j-1) >>= go
        readPrimArray pma (i-1) >>= go >>= writePrimArray pma i
      pure pma
{-# INLINABLE build #-}

step :: Eq a => Table a -> State a -> a -> (Bool, State a)
step (Table xa pa) (State i) x = go i
  where
    go j | indexArray xa j == x =
      if j+1 == sizeofArray xa
      then (,) True $! State (indexPrimArray pa j)
      else (False, State (j+1))
    go 0 = (False, State 0)
    go j = go (indexPrimArray pa (j-1))
{-# INLINABLE step #-}

for_ :: Applicative f => Int -> Int -> (Int -> f a) -> f ()
for_ !i1 !i2 f = go i1
  where
    go i = if i > i2 then pure () else f i *> go (i+1)
{-# INLINE for_ #-}
