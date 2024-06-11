{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.Seqn.Internal.Stream
  ( Step(..)
  , Stream(..)
  , foldr
  , ifoldr
  , isPrefixOf
  , isSubsequenceOf
  , zipWith
  ) where

import Prelude hiding (foldr, zipWith)
import Data.Functor.Classes (Eq1(..), Ord1(..), eq1, compare1)

import qualified Data.Seqn.Internal.Util as U

-- Budget stream fusion
-- The pieces here are adopted from vector-stream. See vector-stream on Hackage
-- for a more complete implementation.

-- Always benchmark and check the Core when making changes to stream stuff!

data Stream a = forall s. Stream (s -> Step s a) s

data Step s a
  = Yield !a s
  | Done

instance Eq a => Eq (Stream a) where
  (==) = eq1
  {-# INLINE (==) #-}

instance Eq1 Stream where
  liftEq f (Stream step1 s10) (Stream step2 s20) = go s10 s20
    where
      go s1 s2 = case step1 s1 of
        Yield x1 s1' -> case step2 s2 of
          Yield x2 s2' -> f x1 x2 && go s1' s2'
          Done -> False
        Done -> case step2 s2 of
          Yield _ _ -> False
          Done -> True
  {-# INLINE liftEq #-}

instance Ord a => Ord (Stream a) where
  compare = compare1
  {-# INLINE compare #-}

instance Ord1 Stream where
  liftCompare f (Stream step1 s10) (Stream step2 s20) = go s10 s20
    where
      go s1 s2 = case step1 s1 of
        Yield x1 s1' -> case step2 s2 of
          Yield x2 s2' -> f x1 x2 <> go s1' s2'
          Done -> GT
        Done -> case step2 s2 of
          Yield _ _ -> LT
          Done -> EQ
  {-# INLINE liftCompare #-}

foldr :: (a -> b -> b) -> b -> Stream a -> b
foldr f z (Stream step s0) = go s0
  where
    go s = case step s of
      Yield x s' -> f x (go s')
      Done -> z
{-# INLINE foldr #-}

ifoldr :: (Int -> a -> b -> b) -> b -> Int -> (Int -> Int) -> Stream a -> b
ifoldr f z i0 istep (Stream step s0) = go i0 s0
  where
    go !i s = case step s of
      Yield x s' -> f i x (go (istep i) s')
      Done -> z
{-# INLINE ifoldr #-}

isPrefixOf :: Eq a => Stream a -> Stream a -> Bool
isPrefixOf (Stream step1 s10) (Stream step2 s20) = go s10 s20
  where
    go s1 s2 = case step1 s1 of
      Yield x1 s1' -> case step2 s2 of
        Yield x2 s2' -> x1 == x2 && go s1' s2'
        Done -> False
      Done -> True
{-# INLINE isPrefixOf #-}

isSubsequenceOf :: Eq a => Stream a -> Stream a -> Bool
isSubsequenceOf (Stream step1 s10) (Stream step2 s20) = go1 s10 s20
  where
    go1 s1 s2 = case step1 s1 of
      Yield x s1' -> go2 x s1' s2
      Done -> True
    go2 !x s1' s2 = case step2 s2 of
      Yield y s2'
        | x == y -> go1 s1' s2'
        | otherwise -> go2 x s1' s2'
      Done -> False
{-# INLINE isSubsequenceOf #-}

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (Stream step1 s10) (Stream step2 s20) = Stream step (U.S2 s10 s20)
  where
    step (U.S2 s1 s2) = case step1 s1 of
      Yield x1 s1' -> case step2 s2 of
        Yield x2 s2' -> Yield (f x1 x2) (U.S2 s1' s2')
        Done -> Done
      Done -> Done
    {-# INLINE [0] step #-}
{-# INLINE zipWith #-}
