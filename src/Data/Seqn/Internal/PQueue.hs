{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- This is an internal module. You probably don't need to import this. Use
-- "Data.Seqn.PQueue" instead.
--
-- = WARNING
--
-- Definitions in this module allow violating invariants that would otherwise be
-- guaranteed by "Data.Seqn.PQueue". Use at your own risk!
--
module Data.Seqn.Internal.PQueue
  (
    -- * PQueue
    PQueue(..)
  , Elem(..)
  , Min(..)
  , empty
  , singleton
  , fromList
  , concatMap
  , insert
  , min
  , minView
  , toSortedList

    -- * Entry
  , Entry(..)
  , entryPrio
  , entryValue
  ) where

import Prelude hiding (concatMap, min)
import Data.Coerce (coerce)
import Control.DeepSeq (NFData(..), NFData1(..))
import qualified Data.Foldable as F
import qualified Data.Foldable.WithIndex as IFo
import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..))
import qualified GHC.Exts as X

import Data.Seqn.MSeq (Measured(..))
import Data.Seqn.Internal.MSeq (MSeq(..))
import qualified Data.Seqn.Internal.MSeq as MSeq
import Data.Seqn.Internal.MTree (MTree(..))
import qualified Data.Seqn.Internal.MTree as T
import qualified Data.Seqn.Internal.Util as U

newtype Min a = Min a

-- Note: We do not use Data.Semigroup.Min because we need a left-biased (<>)
-- for the FIFO property. Data.Semigroup.Min simply delegates to the min
-- function of the underlying type, which is not required to be left-biased.

instance Ord a => Semigroup (Min a) where
  x@(Min x') <> y@(Min y') = if x' <= y' then x else y
  {-# INLINE (<>) #-}

instance NFData a => NFData (Min a) where
  rnf = (coerce :: (a -> ()) -> Min a -> ()) rnf
  {-# INLINABLE rnf #-}

instance NFData1 Min where
  liftRnf = coerce

newtype Elem a = Elem a
  deriving newtype (Eq, Ord, Show, Read, NFData)

instance Ord a => Measured (Elem a) where
  type Measure (Elem a) = Min a
  measure = coerce

-- | A minimum priority queue.
--
-- @PQueue@ can be used as a maximum priority queue by wrapping its elements
-- with t'Data.Ord.Down'.
newtype PQueue a = PQueue (MSeq (Elem a))
  deriving newtype
    (
      -- | Insertion order.
      Eq

      -- | Lexicographical ordering, in insertion order.
    , Ord

      -- |
      -- [@(<>)@]: \(O(\left| \log n_1 - \log n_2 \right|)\). Concatenate
      -- two @PQueue@s.
    , Semigroup

      -- |
      -- [@mempty@]: The empty queue.
    , Monoid

    , Show
    , Read
    )

instance Eq1 PQueue where
  liftEq =
    (coerce :: ((Elem a -> Elem b -> Bool) -> MSeq (Elem a) -> MSeq (Elem b) -> Bool)
            -> (a -> b -> Bool) -> PQueue a -> PQueue b -> Bool)
    liftEq
  {-# INLINE liftEq #-}

instance Ord1 PQueue where
  liftCompare =
    (coerce :: ((Elem a -> Elem b -> Ordering) -> MSeq (Elem a) -> MSeq (Elem b) -> Ordering)
            -> (a -> b -> Ordering) -> PQueue a -> PQueue b -> Ordering)
    liftCompare
  {-# INLINE liftCompare #-}

instance Show1 PQueue where
  liftShowsPrec _ sl _ s = sl (F.toList s)
  {-# INLINE liftShowsPrec #-}

-- |
-- [length]: \(O(1)\).
--
-- Folds in insertion order.
instance Foldable PQueue where
  foldMap =
    (coerce :: ((Elem a -> m) -> MSeq (Elem a) -> m)
            -> (a -> m) -> PQueue a -> m)
    foldMap
  {-# INLINE foldMap #-}

  foldr =
    (coerce :: ((Elem a -> b -> b) -> b -> MSeq (Elem a) -> b)
            -> (a -> b -> b) -> b -> PQueue a -> b)
    foldr
  {-# INLINE foldr #-}

  foldl' =
    (coerce :: ((b -> Elem a -> b) -> b -> MSeq (Elem a) -> b)
            -> (b -> a -> b) -> b -> PQueue a -> b)
    F.foldl'
  {-# INLINE foldl' #-}

  foldl =
    (coerce :: ((b -> Elem a -> b) -> b -> MSeq (Elem a) -> b)
            -> (b -> a -> b) -> b -> PQueue a -> b)
    F.foldl
  {-# INLINE foldl #-}

  foldr' =
    (coerce :: ((Elem a -> b -> b) -> b -> MSeq (Elem a) -> b)
            -> (a -> b -> b) -> b -> PQueue a -> b)
    F.foldr'
  {-# INLINE foldr' #-}

  null = coerce (null @MSeq)
  length = coerce (length @MSeq)

-- | Folds in insertion order.
instance IFo.FoldableWithIndex Int PQueue where
  ifoldMap =
    (coerce :: ((Int -> Elem a -> m) -> MSeq (Elem a) -> m)
            -> (Int -> a -> m) -> PQueue a -> m)
    IFo.ifoldMap
  {-# INLINE ifoldMap #-}

  ifoldr =
    (coerce :: ((Int -> Elem a -> b -> b) -> b -> MSeq (Elem a) -> b)
            -> (Int -> a -> b -> b) -> b -> PQueue a -> b)
    IFo.ifoldr
  {-# INLINE ifoldr #-}

  ifoldr' =
    (coerce :: ((Int -> Elem a -> b -> b) -> b -> MSeq (Elem a) -> b)
            -> (Int -> a -> b -> b) -> b -> PQueue a -> b)
    IFo.ifoldr'
  {-# INLINE ifoldr' #-}

  ifoldl' =
    (coerce :: ((Int -> b -> Elem a -> b) -> b -> MSeq (Elem a) -> b)
            -> (Int -> b -> a -> b) -> b -> PQueue a -> b)
    IFo.ifoldl'
  {-# INLINE ifoldl' #-}

  ifoldl =
    (coerce :: ((Int -> b -> Elem a -> b) -> b -> MSeq (Elem a) -> b)
            -> (Int -> b -> a -> b) -> b -> PQueue a -> b)
    IFo.ifoldl
  {-# INLINE ifoldl #-}

instance Ord a => X.IsList (PQueue a) where
  type Item (PQueue a) = a

  fromList = fromList
  {-# INLINE fromList #-}

  toList = F.toList
  {-# INLINE toList #-}

instance NFData a => NFData (PQueue a) where
  rnf = (coerce :: (MSeq (Elem a) -> ()) -> PQueue a -> ()) rnf
  {-# INLINABLE rnf #-}

instance NFData1 PQueue where
  liftRnf f (PQueue t) = MSeq.liftRnf2 (liftRnf f) (coerce f) t
  {-# INLINE liftRnf #-}

---------------
-- Operations
---------------

-- | The empty queue.
empty :: PQueue a
empty = PQueue MSeq.empty

-- | A singleton queue.
singleton :: a -> PQueue a
singleton = coerce MSeq.singleton

-- | \(O(n)\). Create a queue from a list.
fromList :: Ord a => [a] -> PQueue a
fromList = coerce MSeq.fromList
{-# INLINE fromList #-}

-- | \(O \left(\sum_i \log n_i \right)\).
-- Map over a @Foldable@ and concatenate the results.
concatMap :: (Ord b, Foldable f) => (a -> PQueue b) -> f a -> PQueue b
concatMap =
  (coerce :: ((a -> MSeq (Elem b)) -> f a -> MSeq (Elem b))
          -> (a -> PQueue b) -> f a -> PQueue b)
  MSeq.concatMap
{-# INLINE concatMap #-}

-- | \(O(\log n)\). Insert an element into the queue.
--
-- Note: When inserting multiple elements, it is more efficient to concatenate
-- a fresh queue rather than repeatedly insert elements.
--
-- @
-- q <> fromList xs          -- Good
-- foldl' (flip insert) q xs -- Worse
-- @
insert :: Ord a => a -> PQueue a -> PQueue a
insert = coerce (flip MSeq.snoc)
{-# INLINABLE insert #-}

-- | \(O(1)\). The minimum element in the queue.
min :: Ord a => PQueue a -> Maybe a
min =
  (coerce :: (MSeq (Elem a) -> Maybe (Min a)) -> PQueue a -> Maybe a)
  MSeq.summaryMay
{-# INLINE min #-}

-- | \(O(\log n)\). The minimum element in the queue, with the rest of the
-- queue.
minView :: Ord a => PQueue a -> Maybe (a, PQueue a)
minView (PQueue t) = case t of
  MEmpty -> Nothing
  MTree x xs -> case minViewSure x xs of
    U.S2 y t' -> Just (y, PQueue t')
{-# INLINE minView #-}

minViewSure :: Ord a => Elem a -> MTree (Elem a) -> U.S2 a (MSeq (Elem a))
minViewSure x@(Elem !x1) xs = case xs of
  MTip -> U.S2 x1 MSeq.empty
  MBin _ (Min v) _ _ _
    | x1 <= v -> U.S2 x1 (MSeq.fromMTree xs)
    | otherwise -> U.S2 v (MTree x (deleteSure v xs))
{-# INLINABLE minViewSure #-}

deleteSure :: Ord a => a -> MTree (Elem a) -> MTree (Elem a)
deleteSure !k = \case
  MBin _ _ x@(Elem x1) l r -> case l of
    MTip
      | x1 <= k -> r
      | otherwise -> T.cons x (deleteSure k r)
    MBin _ (Min v) _ _ _
      | v <= k -> T.balanceR x (deleteSure k l) r
      | x1 <= k -> T.glue l r
      | otherwise -> T.balanceL x l (deleteSure k r)
  MTip -> error "PQueue.deleteSure: impossible"
{-# INLINABLE deleteSure #-}

-- | \(O(n \log n)\). Convert to a sorted list.
toSortedList :: Ord a => PQueue a -> [a]
toSortedList q0 = X.build $ \lcons lnil ->
  let go q = case minView q of
        Nothing -> lnil
        Just (x,q') -> lcons x (go q')
  in go q0
{-# INLINE toSortedList #-}

----------
-- Entry
----------

-- | A priority associated with a value. A @PQueue (Entry k a)@ may be used
-- when the priority is separate from the value.
data Entry k a = Entry !k a
  deriving (Show, Read, Functor)

-- | Compares by @k@ only.
instance Eq k => Eq (Entry k a) where
  Entry k1 _ == Entry k2 _ = k1 == k2
  {-# INLINABLE (==) #-}

-- | Compares by @k@ only.
instance Ord k => Ord (Entry k a) where
  compare (Entry k1 _) (Entry k2 _) = compare k1 k2
  {-# INLINABLE compare #-}

  Entry k1 _ <= Entry k2 _ = k1 <= k2
  {-# INLINABLE (<=) #-}

instance (NFData k, NFData a) => NFData (Entry k a) where
  rnf (Entry k x) = rnf k `seq` rnf x
  {-# INLINABLE rnf #-}

-- | The priority.
entryPrio :: Entry k a -> k
entryPrio (Entry k _) = k

-- | The value.
entryValue :: Entry k a -> a
entryValue (Entry _ x) = x
