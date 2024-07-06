{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- This is an internal module. You probably don't need to import this. Use
-- "Data.Seqn.MSeq" instead.
--
-- = WARNING
--
-- Definitions in this module allow violating invariants that would otherwise be
-- guaranteed by "Data.Seqn.MSeq". Use at your own risk!
--
module Data.Seqn.Internal.MSeq
  (
    -- * MSeq
    MSeq(..)

    -- * Construct
  , empty
  , singleton
  , fromList
  , fromRevList
  , replicate
  , replicateA
  , generate
  , generateA
  , unfoldr
  , unfoldl
  , unfoldrM
  , unfoldlM
  , concatMap
  , mfix

    -- * Convert
  , toRevList

    -- * Index
  , lookup
  , index
  , (!?)
  , (!)
  , update
  , adjust
  , insertAt
  , deleteAt

    -- * Slice
  , cons
  , snoc
  , uncons
  , unsnoc
  , take
  , drop
  , slice
  , splitAt
  , takeEnd
  , dropEnd
  , splitAtEnd

    -- * Filter
  , filter
  , mapMaybe
  , mapEither
  , filterA
  , mapMaybeA
  , mapEitherA
  , takeWhile
  , dropWhile
  , span
  , break
  , takeWhileEnd
  , dropWhileEnd
  , spanEnd
  , breakEnd

    -- * Transform
  , map
  , liftA2
  , traverse
  , imap
  , itraverse
  , reverse
  , intersperse
  , scanl
  , scanr
  , sort
  , sortBy

    -- * Search and test
  , findEnd
  , findIndex
  , findIndexEnd
  , infixIndices
  , binarySearchFind
  , isPrefixOf
  , isSuffixOf
  , isInfixOf
  , isSubsequenceOf

    -- * Zip and unzip
  , zipWith
  , zipWith3
  , zipWithM
  , zipWith3M
  , unzipWith
  , unzipWith3

    -- * Measured queries
  , summaryMay
  , summary
  , sliceSummaryMay
  , sliceSummary
  , foldlSliceSummaryComponents
  , binarySearchPrefix
  , binarySearchSuffix

    -- * Force
  , liftRnf2

    -- * Internal
  , fromMTree

    -- * Testing
  , valid
  , debugShowsPrec
  ) where

import Prelude hiding (break, concatMap, drop, dropWhile, filter, liftA2, lookup, map, replicate, reverse, scanl, scanr, span, splitAt, take, takeWhile, traverse, unzip, unzip3, zip, zip3, zipWith, zipWith3)
import qualified Control.Applicative as Ap
import Control.Applicative.Backwards (Backwards(..))
import Control.DeepSeq (NFData(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Coerce (coerce)
import qualified Data.Foldable as F
import qualified Data.Foldable.WithIndex as IFo
import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import qualified Data.Monoid as Monoid
import qualified Data.Primitive.Array as A
import Data.Semigroup (Semigroup(..))
import qualified Data.SamSort as Sam
import qualified GHC.Exts as X
import Text.Read (Read(..))
import qualified Text.Read as Read

import Data.Seqn.Internal.MTree (Measured(..), MTree(..))
import qualified Data.Seqn.Internal.MTree as T
import qualified Data.Seqn.Internal.Util as U
import qualified Data.Seqn.Internal.Stream as Stream
import Data.Seqn.Internal.Stream (Step(..), Stream(..))
import qualified Data.Seqn.Internal.KMP as KMP

--------
-- Seq
--------

-- | A sequence with elements of type @a@. An instance of @'Measured' a@ is
-- required for most operations.
data MSeq a
  = MTree !a !(MTree a)
  | MEmpty
-- See Note [Seq structure] in Data.Seqn.Internal.Seq

--------------
-- Instances
--------------

instance Eq a => Eq (MSeq a) where
  t1 == t2 = compareLength t1 t2 == EQ && stream t1 == stream t2
  {-# INLINABLE (==) #-}

-- | Lexicographical ordering
instance Ord a => Ord (MSeq a) where
  compare t1 t2 = compare (stream t1) (stream t2)
  {-# INLINABLE compare #-}

instance Show a => Show (MSeq a) where
  showsPrec _ t = shows (F.toList t)
  {-# INLINABLE showsPrec #-}

instance (Measured a, Read a) => Read (MSeq a) where
  readPrec = fmap fromList readListPrec
  {-# INLINABLE readPrec #-}

  readListPrec = Read.readListPrecDefault
  {-# INLINABLE readListPrec #-}

instance Eq1 MSeq where
  liftEq f t1 t2 = compareLength t1 t2 == EQ && liftEq f (stream t1) (stream t2)
  {-# INLINE liftEq #-}

instance Ord1 MSeq where
  liftCompare f t1 t2 = liftCompare f (stream t1) (stream t2)
  {-# INLINE liftCompare #-}

instance Show1 MSeq where
  liftShowsPrec _ sl _ t = sl (F.toList t)
  {-# INLINE liftShowsPrec #-}

-- |
-- [@length@]: \(O(1)\).
--
-- Folds are \(O(n)\).
instance Foldable MSeq where
  fold = foldMap id
  {-# INLINABLE fold #-}

  foldMap f = \case
    MTree x xs -> f x <> T.foldMap f xs
    MEmpty -> mempty
  {-# INLINE foldMap #-}

  foldMap' f = F.foldl' (\z x -> z <> f x) mempty
  {-# INLINE foldMap' #-}

  foldr f z = Stream.foldr f z . stream
  {-# INLINE foldr #-}

  foldl f z = Stream.foldr (flip f) z . streamEnd
  {-# INLINE foldl #-}

  foldl' f !z = \case
    MTree x xs -> T.foldl' f (f z x) xs
    MEmpty -> z
  {-# INLINE foldl' #-}

  foldr' f !z = \case
    MTree x xs -> f x $! T.foldr' f z xs
    MEmpty -> z
  {-# INLINE foldr' #-}

  null = \case
    MTree _ _ -> False
    MEmpty -> True

  length = \case
    MTree _ xs -> 1 + T.size xs
    MEmpty -> 0

instance Measured a => X.IsList (MSeq a) where
  type Item (MSeq a) = a
  fromList = fromList
  {-# INLINE fromList #-}

  toList = F.toList
  {-# INLINE toList #-}

instance IFo.FoldableWithIndex Int MSeq where
  ifoldMap f = \case
    MTree x xs -> f 0 x <> T.ifoldMap f 1 xs
    MEmpty -> mempty
  {-# INLINE ifoldMap #-}

  ifoldr f z = Stream.ifoldr f z 0 (+1) . stream
  {-# INLINE ifoldr #-}

  ifoldl f z = \t ->
    Stream.ifoldr (flip . f) z (length t - 1) (subtract 1) (streamEnd t)
  {-# INLINE ifoldl #-}

  ifoldr' f !z = \case
    MTree x xs -> f 0 x $! T.ifoldr' f z (T.size xs) xs
    MEmpty -> z
  {-# INLINE ifoldr' #-}

  ifoldl' f !z = \case
    MTree x xs -> T.ifoldl' f (f 0 z x) 1 xs
    MEmpty -> z
  {-# INLINE ifoldl' #-}

-- |
-- [@(<>)@]: \(O(\left| \log n_1 - \log n_2 \right|)\). Concatenates two
-- sequences.
--
-- [@stimes@]: \(O(\log c)\). @stimes c xs@ is @xs@ repeating @c@ times. If
-- @c < 0@, 'empty' is returned.
instance Measured a => Semigroup (MSeq a) where
  MTree x xs <> MTree y ys = MTree x (T.link y xs ys)
  l <> MEmpty = l
  MEmpty <> r = r
  {-# INLINABLE (<>) #-}

  stimes !c = \case
    t@(MTree x xs)
      | c <= 0 -> MEmpty
      | fromIntegral c * toi (length t) > toi (maxBound :: Int) ->
          error "MSeq.stimes: result size too large"
      | otherwise -> stimesGo x (c'-1) xs xs
      where
        c' = fromIntegral c :: Int
        toi :: Int -> Integer
        toi = fromIntegral
    MEmpty -> MEmpty
  {-# INLINABLE stimes #-}
  -- See Note [Complexity of stimes] in Data.Seqn.Internal.Seq

  sconcat (x:|xs) = mconcat (x:xs)
  {-# INLINABLE sconcat #-}

stimesGo :: Measured a => a -> Int -> MTree a -> MTree a -> MSeq a
stimesGo !x = go
  where
    go c !xs !acc
      | c <= 0 = MTree x acc
      | c `mod` 2 == 0 = go (c `div` 2) (T.bin x xs xs) acc
      | otherwise = go (c `div` 2) (T.bin x xs xs) (T.link x xs acc)
{-# INLINE stimesGo #-}

-- |
-- [@mempty@]: The empty sequence.
instance Measured a => Monoid (MSeq a) where
  mempty = MEmpty

  mconcat = concatMap id
  {-# INLINE mconcat #-} -- Inline for fusion

instance (NFData (Measure a), NFData a) => NFData (MSeq a) where
  rnf = \case
    MTree x xs -> rnf x `seq` rnf xs
    MEmpty -> ()
  {-# INLINABLE rnf #-}

--------------
-- Construct
--------------

-- | The empty sequence.
empty :: MSeq a
empty = MEmpty

-- | A singleton sequence.
singleton :: a -> MSeq a
singleton x = MTree x T.MTip

-- | \(O(n)\). Create an @MSeq@ from a list.
fromList :: Measured a => [a] -> MSeq a
fromList = ltrFinish . F.foldl' ltrPush Nil
{-# INLINE fromList #-}
-- See Note [fromList implementation]

-- | \(O(n)\). Create an @MSeq@ from a reversed list.
fromRevList :: Measured a => [a] -> MSeq a
fromRevList = rtlFinish . F.foldl' (flip rtlPush) Nil
{-# INLINE fromRevList #-}
-- See Note [fromList implementation]

-- | \(O(\log n)\). A sequence with a repeated element.
-- If the length is negative, 'empty' is returned.
replicate :: Measured a => Int -> a -> MSeq a
replicate n !x
  | n <= 0 = MEmpty
  | otherwise = stimesGo x (n-1) MTip MTip
{-# INLINABLE replicate #-}

-- | \(O(n)\). Generate a sequence from a length and an applicative action.
-- If the length is negative, 'empty' is returned.
replicateA :: (Measured a, Applicative f) => Int -> f a -> f (MSeq a)
replicateA !n m = generateA n (const m)
{-# INLINABLE replicateA #-}

-- | \(O(n)\). Generate a sequence from a length and a generator.
-- If the length is negative, 'empty' is returned.
generate :: Measured a => Int -> (Int -> a) -> MSeq a
generate =
  (coerce :: (Int -> (Int -> Identity a) -> Identity (MSeq a))
          -> Int -> (Int -> a) -> MSeq a)
  generateA
{-# INLINE generate #-}

-- | \(O(n)\). Generate a sequence from a length and an applicative generator.
-- If the length is negative, 'empty' is returned.
generateA
  :: (Measured a, Applicative f) => Int -> (Int -> f a) -> f (MSeq a)
generateA n f
  | n <= 0 = pure MEmpty
  | otherwise = Ap.liftA2 MTree (f 0) (T.generateA f 1 (n-1))
{-# INLINE generateA #-}

-- | \(O(n)\). Unfold a sequence from left to right.
unfoldr :: Measured a => (b -> Maybe (a, b)) -> b -> MSeq a
unfoldr =
  (coerce :: ((b -> Identity (Maybe (a, b))) -> b -> Identity (MSeq a))
          -> (b -> Maybe (a, b)) -> b -> MSeq a)
  unfoldrM
{-# INLINE unfoldr #-}

-- | \(O(n)\). Unfold a sequence monadically from left to right.
unfoldrM :: (Measured a, Monad m) => (b -> m (Maybe (a, b))) -> b -> m (MSeq a)
unfoldrM f = go Nil
  where
    go !b z = f z >>= \case
      Nothing -> pure $! ltrFinish b
      Just (x, z') -> go (ltrPush b x) z'
{-# INLINE unfoldrM #-}

-- | \(O(n)\). Unfold a sequence from right to left.
unfoldl :: Measured a => (b -> Maybe (b, a)) -> b -> MSeq a
unfoldl =
  (coerce :: ((b -> Identity (Maybe (b, a))) -> b -> Identity (MSeq a))
          -> (b -> Maybe (b, a)) -> b -> MSeq a)
  unfoldlM
{-# INLINE unfoldl #-}

-- | \(O(n)\). Unfold a sequence monadically from right to left.
unfoldlM :: (Measured a, Monad m) => (b -> m (Maybe (b, a))) -> b -> m (MSeq a)
unfoldlM f = go Nil
  where
    go !b z = f z >>= \case
      Nothing -> pure $! rtlFinish b
      Just (z', x) -> go (rtlPush x b) z'
{-# INLINE unfoldlM #-}

-- | \(O \left(\sum_i \log n_i \right)\).
-- Map over a @Foldable@ and concatenate the results.
concatMap :: (Measured b, Foldable f) => (a -> MSeq b) -> f a -> MSeq b
concatMap f = ltrFinish . F.foldl' g Nil
  where
    g b x = case f x of
      MEmpty -> b
      MTree y ys -> ltrPushMany b y ys
    {-# INLINE g #-}
{-# INLINE concatMap #-}
-- See Note [concatMap implementation]

-- | Monadic fixed point. See "Control.Monad.Fix".
mfix :: Measured a => (a -> MSeq a) -> MSeq a
mfix f =
  imap
    (\i _ -> let x = index i (f x) in x)
    (f (error "MSeq.mfix: f must be lazy"))
{-# INLINE mfix #-}

------------
-- Convert
------------

-- | \(O(n)\). Convert to a list in reverse.
--
-- To convert to a list without reversing, use
-- @Data.Foldable.'Data.Foldable.toList'@.
toRevList :: MSeq a -> [a]
toRevList t = X.build $ \lcons lnil -> F.foldl (flip lcons) lnil t
{-# INLINE toRevList #-}

----------
-- Index
----------

-- | \(O(\log n)\). Look up the element at an index.
lookup :: Int -> MSeq a -> Maybe a
lookup !i (MTree x xs)
  | i < 0 || T.size xs < i = Nothing
  | i == 0 = Just x
  | otherwise = Just $! T.index (i-1) xs
lookup _ MEmpty = Nothing
{-# INLINE lookup #-}

-- | \(O(\log n)\). Look up the element at an index. Calls @error@ if the index
-- is out of bounds.
index :: Int -> MSeq a -> a
index !i = \case
  MTree x xs
    | i == 0 -> x
    | otherwise -> T.index (i-1) xs
  MEmpty -> error "MSeq.index: out of bounds"

-- | \(O(\log n)\). Infix version of 'lookup'.
(!?) :: MSeq a -> Int -> Maybe a
(!?) = flip lookup
{-# INLINE (!?) #-}

-- | \(O(\log n)\). Infix version of 'index'. Calls @error@ if the index is out
-- of bounds.
(!) :: MSeq a -> Int -> a
(!) = flip index

-- | \(O(\log n)\). Update an element at an index. If the index is out of
-- bounds, the sequence is returned unchanged.
update :: Measured a => Int -> a -> MSeq a -> MSeq a
update i x t = adjust (const x) i t
{-# INLINABLE update #-}

-- | \(O(\log n)\). Adjust the element at an index. If the index is out of
-- bounds, the sequence is returned unchanged.
adjust :: Measured a => (a -> a) -> Int -> MSeq a -> MSeq a
adjust f !i t = case t of
  MTree x xs
    | i < 0 || T.size xs < i -> t
    | i == 0 -> MTree (f x) xs
    | otherwise -> MTree x (runIdentity (T.adjustF (Identity U.#. f) (i-1) xs))
  MEmpty -> MEmpty
{-# INLINE adjust #-}

-- | \(O(\log n)\). Insert an element at an index. If the index is out of
-- bounds, the element is added to the closest end of the sequence.
insertAt :: Measured a => Int -> a -> MSeq a -> MSeq a
insertAt !i y t = case t of
  MTree x xs
    | i <= 0 -> cons y t
    | otherwise -> MTree x (T.insertAt (i-1) y xs)
  MEmpty -> singleton y
{-# INLINABLE insertAt #-}

-- | \(O(\log n)\). Delete an element at an index. If the index is out of
-- bounds, the sequence is returned unchanged.
deleteAt :: Measured a => Int -> MSeq a -> MSeq a
deleteAt !i t = case t of
  MTree x xs
    | i < 0 || T.size xs < i -> t
    | i == 0 -> fromMTree xs
    | otherwise -> MTree x (T.deleteAt (i-1) xs)
  MEmpty -> MEmpty
{-# INLINABLE deleteAt #-}

----------
-- Slice
----------

-- | \(O(\log n)\). Append a value to the beginning of a sequence.
cons :: Measured a => a -> MSeq a -> MSeq a
cons x (MTree y ys) = MTree x (T.cons y ys)
cons x MEmpty = singleton x
{-# INLINABLE cons #-}

-- | \(O(\log n)\). Append a value to the end of a sequence.
snoc :: Measured a => MSeq a -> a -> MSeq a
snoc (MTree y ys) x = MTree y (T.snoc ys x)
snoc MEmpty x = singleton x
{-# INLINABLE snoc #-}

-- | \(O(\log n)\). The head and tail of a sequence.
uncons :: Measured a => MSeq a -> Maybe (a, MSeq a)
uncons (MTree x xs) = Just . (,) x $! fromMTree xs
uncons MEmpty = Nothing
{-# INLINE uncons #-}

-- | \(O(\log n)\). The init and last of a sequence.
unsnoc :: Measured a => MSeq a -> Maybe (MSeq a, a)
unsnoc (MTree x xs) = case T.unsnoc xs of
  U.SNothing -> Just (MEmpty, x)
  U.SJust (U.S2 ys y) -> Just (MTree x ys, y)
unsnoc MEmpty = Nothing
{-# INLINE unsnoc #-}

-- | \(O(\log n)\). Take a number of elements from the beginning of a sequence.
take :: Measured a => Int -> MSeq a -> MSeq a
take !i t@(MTree x xs)
  | i <= 0 = MEmpty
  | T.size xs < i = t
  | otherwise = MTree x (getConst (T.splitAtF (i-1) xs))
take _ MEmpty = MEmpty
{-# INLINABLE take #-}

-- | \(O(\log n)\). Drop a number of elements from the beginning of a sequence.
drop :: Measured a => Int -> MSeq a -> MSeq a
drop !i t@(MTree _ xs)
  | i <= 0 = t
  | T.size xs < i = MEmpty
  | otherwise = case U.unTagged (T.splitAtF (i-1) xs) of
      U.S2 x' xs' -> MTree x' xs'
drop _ MEmpty = MEmpty
{-# INLINABLE drop #-}

-- | \(O(\log n)\). The slice of a sequence between two indices (inclusive).
slice :: Measured a => (Int, Int) -> MSeq a -> MSeq a
slice (i,j) = drop i . take (j+1)
{-# INLINABLE slice #-}

-- | \(O(\log n)\). Take a number of elements from the end of a sequence.
takeEnd :: Measured a => Int -> MSeq a -> MSeq a
takeEnd n t = drop (length t - n) t
{-# INLINABLE takeEnd #-}

-- | \(O(\log n)\). Drop a number of elements from the end of a sequence.
dropEnd :: Measured a => Int -> MSeq a -> MSeq a
dropEnd n t = take (length t - n) t
{-# INLINABLE dropEnd #-}

-- | \(O(\log n)\). Split a sequence at a given index.
--
-- @splitAt n xs == ('take' n xs, 'drop' n xs)@
splitAt :: Measured a => Int -> MSeq a -> (MSeq a, MSeq a)
splitAt !i t@(MTree x xs)
  | i <= 0 = (MEmpty, t)
  | T.size xs < i = (t, MEmpty)
  | otherwise = case T.splitAtF (i-1) xs of
      U.S2 xs1 (U.S2 x' xs2) -> (MTree x xs1, MTree x' xs2)
splitAt _ MEmpty = (MEmpty, MEmpty)
{-# INLINABLE splitAt #-}

-- | \(O(\log n)\). Split a sequence at a given index from the end.
--
-- @splitAtEnd n xs == ('dropEnd' n xs, 'takeEnd' n xs)@
splitAtEnd :: Measured a => Int -> MSeq a -> (MSeq a, MSeq a)
splitAtEnd i s = splitAt (length s - i) s
{-# INLINABLE splitAtEnd #-}

-----------
-- Filter
-----------

-- | \(O(n)\). Keep elements that satisfy a predicate.
filter :: Measured a => (a -> Bool) -> MSeq a -> MSeq a
filter =
  (coerce :: ((a -> Identity Bool) -> MSeq a -> Identity (MSeq a))
          -> (a -> Bool) -> MSeq a -> MSeq a)
  filterA
{-# INLINE filter #-}

-- | \(O(n)\). Map over elements and collect the @Just@s.
mapMaybe :: Measured b => (a -> Maybe b) -> MSeq a -> MSeq b
mapMaybe =
  (coerce :: ((a -> Identity (Maybe b)) -> MSeq a -> Identity (MSeq b))
          -> (a -> Maybe b) -> MSeq a -> MSeq b)
  mapMaybeA
{-# INLINE mapMaybe #-}

-- | \(O(n)\). Map over elements and split the @Left@s and @Right@s.
mapEither
  :: (Measured b, Measured c) => (a -> Either b c) -> MSeq a -> (MSeq b, MSeq c)
mapEither =
  (coerce :: ((a -> Identity (Either b c)) -> MSeq a -> Identity (MSeq b, MSeq c))
          -> (a -> Either b c) -> MSeq a -> (MSeq b, MSeq c))
  mapEitherA
{-# INLINE mapEither #-}

-- | \(O(n)\). Keep elements that satisfy an applicative predicate.
filterA :: (Measured a, Applicative f) => (a -> f Bool) -> MSeq a -> f (MSeq a)
filterA f = mapMaybeA (\x -> fmap (\b -> if b then Just x else Nothing) (f x))
{-# INLINE filterA #-}

-- | \(O(n)\). Traverse over elements and collect the @Just@s.
mapMaybeA
  :: (Measured b, Applicative f) => (a -> f (Maybe b)) -> MSeq a -> f (MSeq b)
mapMaybeA f = \case
  MTree x xs -> Ap.liftA2 (maybe fromMTree MTree) (f x) (T.mapMaybeA f xs)
  MEmpty -> pure MEmpty
{-# INLINE mapMaybeA #-}

-- | \(O(n)\). Traverse over elements and split the @Left@s and @Right@s.
mapEitherA
  :: (Measured b, Measured c, Applicative f)
  => (a -> f (Either b c)) -> MSeq a -> f (MSeq b, MSeq c)
mapEitherA f = \case
  MTree x xs -> (\g -> Ap.liftA2 g (f x) (T.mapEitherA f xs)) $ \mx xs' ->
    case mx of
      Left x' -> unS2 $ bimap (MTree x') fromMTree xs'
      Right x' -> unS2 $ bimap fromMTree (MTree x') xs'
  MEmpty -> pure (MEmpty, MEmpty)
  where
    unS2 (U.S2 x y) = (x, y)
{-# INLINE mapEitherA #-}

-- | \(O(i + \log n)\). The longest prefix of elements that satisfy a predicate.
-- \(i\) is the length of the prefix.
takeWhile :: Measured a => (a -> Bool) -> MSeq a -> MSeq a
takeWhile p t = IFo.ifoldr (\i x z -> if p x then z else take i t) t t
{-# INLINE takeWhile #-}

-- | \(O(i + \log n)\). The remainder after removing the longest prefix of
-- elements that satisfy a predicate.
-- \(i\) is the length of the prefix.
dropWhile :: Measured a => (a -> Bool) -> MSeq a -> MSeq a
dropWhile p t = IFo.ifoldr (\i x z -> if p x then z else drop i t) MEmpty t
{-# INLINE dropWhile #-}

-- | \(O(i + \log n)\). The longest prefix of elements that satisfy a predicate,
-- together with the remainder of the sequence.
-- \(i\) is the length of the prefix.
--
-- @span p xs == ('takeWhile' p xs, 'dropWhile' p xs)@
span :: Measured a => (a -> Bool) -> MSeq a -> (MSeq a, MSeq a)
span p t = IFo.ifoldr (\i x z -> if p x then z else splitAt i t) (t, MEmpty) t
{-# INLINE span #-}

-- | \(O(i + \log n)\). The longest prefix of elements that /do not/ satisfy a
-- predicate, together with the remainder of the sequence. \(i\) is the length
-- of the prefix.
--
-- @break p == 'span' (not . p)@
break :: Measured a => (a -> Bool) -> MSeq a -> (MSeq a, MSeq a)
break p = span (not . p)
{-# INLINE break #-}

-- | \(O(i + \log n)\). The longest suffix of elements that satisfy a predicate.
-- \(i\) is the length of the suffix.
takeWhileEnd :: Measured a => (a -> Bool) -> MSeq a -> MSeq a
takeWhileEnd p t = IFo.ifoldl (\i z x -> if p x then z else drop (i+1) t) t t
{-# INLINE takeWhileEnd #-}

-- | \(O(i + \log n)\). The remainder after removing the longest suffix of
-- elements that satisfy a predicate.
-- \(i\) is the length of the suffix.
dropWhileEnd :: Measured a => (a -> Bool) -> MSeq a -> MSeq a
dropWhileEnd p t =
  IFo.ifoldl (\i z x -> if p x then z else take (i+1) t) MEmpty t
{-# INLINE dropWhileEnd #-}

-- | \(O(i + \log n)\). The longest suffix of elements that satisfy a predicate,
-- together with the remainder of the sequence.
-- \(i\) is the length of the suffix.
--
-- @spanEnd p xs == ('dropWhileEnd' p xs, 'takeWhileEnd' p xs)@
spanEnd :: Measured a => (a -> Bool) -> MSeq a -> (MSeq a, MSeq a)
spanEnd p t =
  IFo.ifoldl (\i z x -> if p x then z else splitAt (i+1) t) (MEmpty, t) t
{-# INLINE spanEnd #-}

-- | \(O(i + \log n)\). The longest suffix of elements that /do not/ satisfy a
-- predicate, together with the remainder of the sequence.
-- \(i\) is the length of the suffix.
--
-- @breakEnd p == 'spanEnd' (not . p)@
breakEnd :: Measured a => (a -> Bool) -> MSeq a -> (MSeq a, MSeq a)
breakEnd p = spanEnd (not . p)
{-# INLINE breakEnd #-}

--------------
-- Transform
--------------

-- Note [Functor MSeq]
-- ~~~~~~~~~~~~~~~~~~~
-- MSeq cannot be a Functor because of the Measured constraint on the element
-- type. So class methods which require Functor are provided as standalone.

-- | \(O(n)\). Map over a sequence.
map :: Measured b => (a -> b) -> MSeq a -> MSeq b
map =
  (coerce :: ((a -> Identity b) -> MSeq a -> Identity (MSeq b))
          -> (a -> b) -> MSeq a -> MSeq b)
  traverse
{-# INLINE map #-}

-- | \(O(n_1 n_2)\). Cartesian product of two sequences.
liftA2 :: Measured c => (a -> b -> c) -> MSeq a -> MSeq b -> MSeq c
liftA2 f t1 t2 = case t2 of
  MEmpty -> MEmpty
  MTree x MTip -> map (`f` x) t1
  _ -> concatMap (\x -> map (f x) t2) t1
{-# INLINE liftA2 #-}

-- | \(O(n)\). Traverse a sequence.
traverse
  :: (Measured b, Applicative f) => (a -> f b) -> MSeq a -> f (MSeq b)
traverse f = \case
  MEmpty -> pure MEmpty
  MTree x xs -> Ap.liftA2 MTree (f x) (T.traverse f xs)
{-# INLINE traverse #-}

-- | \(O(n)\). Map over a sequence with index.
imap :: Measured b => (Int -> a -> b) -> MSeq a -> MSeq b
imap =
  (coerce :: ((Int -> a -> Identity b) -> MSeq a -> Identity (MSeq b))
          -> (Int -> a -> b) -> MSeq a -> MSeq b)
  itraverse
{-# INLINE imap #-}

-- | \(O(n)\). Traverse a sequence with index.
itraverse
  :: (Measured b, Applicative f) => (Int -> a -> f b) -> MSeq a -> f (MSeq b)
itraverse f = \case
  MEmpty -> pure MEmpty
  MTree x xs -> Ap.liftA2 MTree (f 0 x) (T.itraverse f 1 xs)
{-# INLINE itraverse #-}

-- | \(O(n)\). Reverse a sequence.
reverse :: Measured a => MSeq a -> MSeq a
reverse (MTree x xs) = case T.uncons (rev xs) of
  U.SNothing -> MTree x MTip
  U.SJust (U.S2 x' xs') -> MTree x' (T.snoc xs' x)
  where
    rev T.MTip = T.MTip
    rev (T.MBin sz _ y l r) = T.binn sz y (rev r) (rev l)
reverse MEmpty = MEmpty
{-# INLINABLE reverse #-}

-- | \(O(n)\). Intersperse an element between the elements of a sequence.
intersperse :: Measured a => a -> MSeq a -> MSeq a
intersperse y (MTree x xs) = case T.unsnoc (go xs) of
  U.SNothing -> error "intersperse: impossible"
  U.SJust (U.S2 xs' _) -> MTree x xs'
  where
    go T.MTip = T.singleton y
    go (T.MBin sz _ z l r) = T.binn (sz*2+1) z (go l) (go r)
    -- No need to balance, x <= 3y => 2x+1 <= 3(2y+1)
intersperse _ MEmpty = MEmpty
{-# INLINABLE intersperse #-}

-- | \(O(n)\). Like 'Data.Foldable.foldl'' but keeps all intermediate values.
scanl :: Measured b => (b -> a -> b) -> b -> MSeq a -> MSeq b
scanl f !z0 =
  cons z0 .
  flip U.evalSState z0 .
  traverse (\x -> U.sState (\z -> let z' = f z x in U.S2 z' z'))
{-# INLINE scanl #-}
-- See Note [SState for scans] in Data.Seqn.Internal.Seq

-- | \(O(n)\). Like 'Data.Foldable.foldr'' but keeps all intermediate values.
scanr :: Measured b => (a -> b -> b) -> b -> MSeq a -> MSeq b
scanr f !z0 =
  flip snoc z0 .
  flip U.evalSState z0 .
  forwards .
  traverse
    (\x -> Backwards (U.sState (\z -> let z' = f x z in U.S2 z' z')))
{-# INLINE scanr #-}

-- | \(O(n \log n)\). Sort a sequence.
sort :: (Ord a, Measured a) => MSeq a -> MSeq a
sort = sortBy compare
{-# INLINABLE sort #-}

-- | \(O(n \log n)\). Sort a sequence using a comparison function.
sortBy :: Measured a => (a -> a -> Ordering) -> MSeq a -> MSeq a
sortBy cmp xs = imap (\i _ -> A.indexArray xa i) xs
  where
    n = length xs
    xa = A.createArray n errorElement $ \ma@(A.MutableArray ma#) -> do
      IFo.ifoldr (\i x z -> A.writeArray ma i x *> z) (pure ()) xs
      Sam.sortArrayBy cmp ma# 0 n
{-# INLINABLE sortBy #-}
-- See Note [Inlinable sortBy] in Data.Seqn.Internal.Seq

--------------------
-- Search and test
--------------------

-- | \(O(n)\). The last element satisfying a predicate.
--
-- To get the first element, use @Data.Foldable.'Data.Foldable.find'@.
findEnd :: (a -> Bool) -> MSeq a -> Maybe a
findEnd f =
  Monoid.getLast . foldMap (\x -> Monoid.Last (if f x then Just x else Nothing))
{-# INLINE findEnd #-}

-- | \(O(n)\). The index of the first element satisfying a predicate.
findIndex :: (a -> Bool) -> MSeq a -> Maybe Int
findIndex f =
  Monoid.getFirst .
  IFo.ifoldMap (\i x -> Monoid.First (if f x then Just i else Nothing))
{-# INLINE findIndex #-}

-- | \(O(n)\). The index of the last element satisfying a predicate.
findIndexEnd :: (a -> Bool) -> MSeq a -> Maybe Int
findIndexEnd f =
  Monoid.getLast .
  IFo.ifoldMap (\i x -> Monoid.Last (if f x then Just i else Nothing))
{-# INLINE findIndexEnd #-}

-- | \(O(n_1 + n_2)\). Indices in the second sequence where the first sequence
-- begins as a substring. Includes overlapping occurences.
infixIndices :: Eq a => MSeq a -> MSeq a -> [Int]
infixIndices t1 t2
  | null t1 = [0 .. length t2]
  | compareLength t1 t2 == GT = []
  | otherwise = X.build $ \lcons lnil ->
    let n1 = length t1
        t1a = infixIndicesMkArray n1 t1
        !(!mt, !mt0) = KMP.build t1a
        f !i x k = \ !m -> case KMP.step mt m x of
          (b,m') ->
            if b
            then lcons (i-n1+1) (k m')
            else k m'
    in IFo.ifoldr f (\ !_ -> lnil) t2 mt0
{-# INLINE infixIndices #-} -- Inline for fusion

infixIndicesMkArray :: Int -> MSeq a -> A.Array a
infixIndicesMkArray !n !t = A.createArray n errorElement $ \ma ->
  IFo.ifoldr (\i x z -> A.writeArray ma i x *> z) (pure ()) t

-- | \(O(\log n)\). Binary search for an element in a sequence.
--
-- Given a function @f@ this function returns an arbitrary element @x@, if it
-- exists, such that @f x = EQ@. @f@ must be monotonic on the sequence—
-- specifically @fmap f@ must result in a sequence which has many (possibly
-- zero) @LT@s, followed by many @EQ@s, followed by many @GT@s.
binarySearchFind :: (a -> Ordering) -> MSeq a -> Maybe a
binarySearchFind f = \case
  MEmpty -> Nothing
  MTree x xs -> case f x of
    LT -> go xs
    EQ -> Just x
    GT -> Nothing
  where
    go MTip = Nothing
    go (MBin _ _ y l r) = case f y of
      LT -> go r
      EQ -> Just y
      GT -> go l
{-# INLINE binarySearchFind #-}

-- | \(O(\min(n_1,n_2))\). Whether the first sequence is a prefix of the second.
isPrefixOf :: Eq a => MSeq a -> MSeq a -> Bool
isPrefixOf t1 t2 =
  compareLength t1 t2 /= GT && Stream.isPrefixOf (stream t1) (stream t2)
{-# INLINABLE isPrefixOf #-}

-- | \(O(\min(n_1,n_2))\). Whether the first sequence is a suffix of the second.
isSuffixOf :: Eq a => MSeq a -> MSeq a -> Bool
isSuffixOf t1 t2 =
  compareLength t1 t2 /= GT && Stream.isPrefixOf (streamEnd t1) (streamEnd t2)
{-# INLINABLE isSuffixOf #-}

-- | \(O(n_1 + n_2)\). Whether the first sequence is a substring of the second.
isInfixOf :: Eq a => MSeq a -> MSeq a -> Bool
isInfixOf t1 t2 = not (null (infixIndices t1 t2))
{-# INLINABLE isInfixOf #-}

-- | \(O(n_1 + n_2)\). Whether the first sequence is a subsequence of the second.
isSubsequenceOf :: Eq a => MSeq a -> MSeq a -> Bool
isSubsequenceOf t1 t2 =
  compareLength t1 t2 /= GT && Stream.isSubsequenceOf (stream t1) (stream t2)
{-# INLINABLE isSubsequenceOf #-}

------------------
-- Zip and unzip
------------------

-- | \(O(\min(n_1,n_2))\). Zip two sequences with a function. The result is
-- as long as the shorter sequence.
zipWith :: Measured c => (a -> b -> c) -> MSeq a -> MSeq b -> MSeq c
zipWith =
  (coerce :: ((a -> b -> Identity c) -> MSeq a -> MSeq b -> Identity (MSeq c))
          -> (a -> b -> c) -> MSeq a -> MSeq b -> MSeq c)
  zipWithM
{-# INLINE zipWith #-}

-- | \(O(\min(n_1,n_2,n_3))\). Zip three sequences with a function. The result
-- is as long as the shortest sequence.
zipWith3
  :: Measured d => (a -> b -> c -> d) -> MSeq a -> MSeq b -> MSeq c -> MSeq d
zipWith3 =
  (coerce :: ((a -> b -> c -> Identity d) -> MSeq a -> MSeq b -> MSeq c -> Identity (MSeq d))
          -> (a -> b -> c -> d) -> MSeq a -> MSeq b -> MSeq c -> MSeq d)
  zipWith3M
{-# INLINE zipWith3 #-}

-- | \(O(\min(n_1,n_2))\). Zip two sequences with a monadic function.
zipWithM
  :: (Measured c, Monad m) => (a -> b -> m c) -> MSeq a -> MSeq b -> m (MSeq c)
zipWithM f t1 t2 = zipWithStreamM f t1 (stream t2)
{-# INLINE zipWithM #-}

-- | \(O(\min(n_1,n_2,n_3))\). Zip three sequences with a monadic function.
zipWith3M
  :: (Measured d, Monad m)
  => (a -> b -> c -> m d) -> MSeq a -> MSeq b -> MSeq c -> m (MSeq d)
zipWith3M f t1 t2 t3 =
  zipWithStreamM
    (\x (U.S2 y z) -> f x y z)
    t1
    (Stream.zipWith U.S2 (stream t2) (stream t3))
{-# INLINE zipWith3M #-}

zipWithStreamM
  :: (Measured c, Monad m)
  => (a -> b -> m c) -> MSeq a -> Stream b -> m (MSeq c)
zipWithStreamM f t strm = case t of
  MEmpty -> pure MEmpty
  MTree x xs -> case strm of
    Stream step s -> case step s of
      Done -> pure MEmpty
      Yield y s1 ->
        Ap.liftA2 MTree (f x y) (T.zipWithStreamM f xs (Stream step s1))
{-# INLINE zipWithStreamM #-}

-- | \(O(n)\). Map over a sequence and unzip the result.
unzipWith
  :: (Measured b, Measured c)
  => (a -> (b, c)) -> MSeq a -> (MSeq b, MSeq c)
unzipWith f t = case t of
  MTree x xs ->
    case (f x, T.unzipWithA (Identity U.#. f) xs) of
      ((x1,x2), Identity (U.S2 xs1 xs2)) ->
        let !t1 = MTree x1 xs1
            !t2 = MTree x2 xs2
        in (t1,t2)
  MEmpty -> (MEmpty, MEmpty)
{-# INLINE unzipWith #-}

-- | \(O(n)\). Map over a sequence and unzip the result.
unzipWith3
  :: (Measured b, Measured c, Measured d)
  => (a -> (b, c, d)) -> MSeq a -> (MSeq b, MSeq c, MSeq d)
unzipWith3 f t = case t of
  MTree x xs ->
    case (f x, T.unzipWith3A (Identity U.#. f) xs) of
      ((x1,x2,x3), Identity (U.S3 xs1 xs2 xs3)) ->
        let !t1 = MTree x1 xs1
            !t2 = MTree x2 xs2
            !t3 = MTree x3 xs3
        in (t1,t2,t3)
  MEmpty -> (MEmpty, MEmpty, MEmpty)
{-# INLINE unzipWith3 #-}

---------------------
-- Measured queries
---------------------

-- | \(O(1)\). The summary is the fold of measures of all elements in the
-- sequence. Returns @Nothing@ if the sequence is empty.
--
-- @summaryMay == 'foldMap' (Just . 'measure')@
summaryMay :: Measured a => MSeq a -> Maybe (Measure a)
summaryMay t = case t of
  MTree x xs -> Just $! measure x T.<<> xs
  MEmpty -> Nothing
{-# INLINE summaryMay #-}

-- | \(O(1)\). The summary is the fold of measures of all elements in the
-- sequence.
--
-- @summary == 'foldMap' 'measure'@
summary :: (Measured a, Monoid (Measure a)) => MSeq a -> Measure a
summary t = fromMaybe mempty (summaryMay t)
{-# INLINABLE summary #-}

-- | \(O(\log n)\). The summary of a slice of the sequence. The slice is
-- indicated by its bounds (inclusive).
--
-- @sliceSummaryMay lu == 'summaryMay' . 'slice' lu@
sliceSummaryMay :: Measured a => (Int, Int) -> MSeq a -> Maybe (Measure a)
sliceSummaryMay (!ql, !qu) t = case t of
  MEmpty -> Nothing
  MTree x xs
    | ql > qu || qu < 0 || length t - 1 < ql -> Nothing
    | otherwise -> Just $! foldlMap1SliceSummaryComponents id (<>) ql qu x xs
{-# INLINE sliceSummaryMay #-}

-- | \(O(\log n)\). The summary of a slice of the sequence. The slice is
-- indicated by its bounds (inclusive).
--
-- @sliceSummary lu == 'summary' . 'slice' lu@
sliceSummary
  :: (Measured a, Monoid (Measure a)) => (Int, Int) -> MSeq a -> Measure a
sliceSummary lu t = fromMaybe mempty (sliceSummaryMay lu t)
{-# INLINABLE sliceSummary #-}

-- | Strict left fold over measures covering a slice. These measures are
-- summaries of \(O(\log n)\) adjacent slices which form the requested slice
-- when concatenated.
--
-- @foldlSliceSummaryComponents (<>) mempty == 'sliceSummary'@
--
-- This function is useful when
--
-- * Some property of the summary of a slice is desired.
-- * It is expensive to compute the summary, i.e. @(<>)@ for @Measure a@ is
--   expensive.
-- * It is possible, and cheaper, to compute the property given components
--   of the summary of the slice.
--
-- ==== __Examples__
--
-- One use case for this is order statistic queries on a slice, such as counting
-- the number of elements less than some value.
--
-- It requires a @Multiset@ structure as outlined below, which can be
-- implemented using sorted arrays/balanced binary trees.
--
-- @
-- data Multiset a
-- singleton :: Ord a => a -> MultiSet a -- O(1)
-- (<>) :: Ord a => Multiset a -> Multiset a -> Multiset a -- O(n1 + n2)
-- countLessThan :: Ord a => a -> Multiset a -> Int -- O(log n)
-- @
--
-- @
-- import Data.Seqn.MSeq (Measured, MSeq)
-- import qualified Data.Seqn.MSeq as MSeq
--
-- newtype Elem a = Elem a deriving Show
--
-- instance Ord a => Measured (Elem a) where
--   type Measure (Elem x) = Multiset x
--   measure (Elem x) = singleton x
--
-- -- | O(n log n).
-- fromList :: Ord a => [a] -> MSeq (Elem a)
-- fromList = MSeq.fromList
--
-- -- | O(log^2 n).
-- countLessThanInSlice :: Ord a => a -> (Int, Int) -> MSeq (Elem a) -> Int
-- countLessThanInSlice k =
--   MSeq.foldlSliceSummaryComponents (\\acc xs -> acc + countLessThan k xs) 0
-- @
foldlSliceSummaryComponents
  :: Measured a => (b -> Measure a -> b) -> b -> (Int, Int) -> MSeq a -> b
foldlSliceSummaryComponents f !z (!ql, !qu) t = case t of
  MEmpty -> z
  MTree x xs
    | ql > qu || qu < 0 || length t - 1 < ql -> z
    | otherwise -> foldlMap1SliceSummaryComponents (f z) f ql qu x xs
{-# INLINE foldlSliceSummaryComponents #-}

-- Precondition: slice (ql, qu) (Tree x0 xs0) is non-empty
foldlMap1SliceSummaryComponents
  :: Measured a
  => (Measure a -> b) -> (b -> Measure a -> b)
  -> Int -> Int -> a -> MTree a -> b
foldlMap1SliceSummaryComponents f g !ql !qu x0 xs0
  | ql <= 0 && 0 <= qu = go (f (measure x0)) 1 xs0
  | otherwise = go1 1 xs0
  where
    go1 !i (MBin sz v y l r)
      | ql <= i && k <= qu = f v
      | ql < j && i <= qu =
          if ql <= j && j <= qu
          then go (g (go1 i l) (measure y)) (j+1) r
          else go1 i l
      | ql <= j && j <= qu = go (f (measure y)) (j+1) r
      | otherwise = go1 (j+1) r
      where
        k = i + sz - 1
        j = i + T.size l
    go1 _ MTip = error "MSeq.foldlMap1SliceSummaryComponents: impossible"

    go !z !i (MBin sz v x l r)
      | qu < i || k < ql = z
      | ql <= i && k <= qu = g z v
      | ql <= j && j <= qu = go (g (go z i l) (measure x)) (j+1) r
      | otherwise = go (go z i l) (j+1) r
      where
        k = i + sz - 1
        j = i + T.size l
    go z _ MTip = z
{-# INLINE foldlMap1SliceSummaryComponents #-}

-- | \(O(\log n)\). Perform a binary search on the summaries of the non-empty
-- prefixes of the sequence.
--
-- @binarySearchPrefix p xs@ for a monotonic predicate @p@ returns two adjacent
-- indices @i@ and @j@, @0 <= i < j < length xs@.
--
-- * @i@ is the greatest index such that
--   @p (fromJust (summaryMay (take (i+1) xs)))@
--   is @False@, or @Nothing@ if there is no such index.
-- * @j@ is the least index such that
--   @p (fromJust (summaryMay (take (j+1) xs)))@
--   is @True@, or @Nothing@ if there is no such index.
--
-- ==== __Examples__
--
-- @
-- import "Data.Monoid" (Sum(..))
--
-- newtype Elem = E Int deriving Show
--
-- instance Measured Elem where
--   type Measure Elem = Sum Int
--   measure (E x) = Sum x
-- @
--
-- >>> let xs = fromList [E 1, E 2, E 3, E 4]
--
-- The summaries of the prefixes of @xs@ by index are:
--
-- * @0: measure (E 1) = Sum 1@.
-- * @1: measure (E 1) <> measure (E 2) = Sum 3@.
-- * @2: measure (E 1) <> measure (E 2) <> measure (E 3) = Sum 6@.
-- * @3: measure (E 1) <> measure (E 2) <> measure (E 3) <> measure (E 4) = Sum 10@.
--
-- >>> binarySearchPrefix (> Sum 4) xs
-- (Just 1,Just 2)
--
-- @
--                  ╭──────────┬──────────┬──────────┬──────────╮
-- index:           │        0 │        1 │        2 │        3 │
--                  ├──────────┼──────────┼──────────┼──────────┤
-- prefix summary:  │    Sum 1 │    Sum 3 │    Sum 6 |   Sum 10 │
--                  ├──────────┼──────────┼──────────┼──────────┤
-- (> Sum 4):       │    False │    False │     True │     True │
--                  ╰──────────┴──────────┴──────────┴──────────╯
-- result:                       ( Just 1 ,   Just 2 )
-- @
--
-- >>> binarySearchPrefix (> Sum 20) xs
-- (Just 3,Nothing)
--
-- @
--                  ╭──────────┬──────────┬──────────┬──────────╮
-- index:           │        0 │        1 │        2 │        3 │
--                  ├──────────┼──────────┼──────────┼──────────┤
-- prefix summary:  │    Sum 1 │    Sum 3 │    Sum 6 |   Sum 10 │
--                  ├──────────┼──────────┼──────────┼──────────┤
-- (> Sum 20):      │    False │    False │    False │    False │
--                  ╰──────────┴──────────┴──────────┴──────────╯
-- result:                                             ( Just 3 ,  Nothing )
-- @
--
binarySearchPrefix
  :: Measured a => (Measure a -> Bool) -> MSeq a -> (Maybe Int, Maybe Int)
binarySearchPrefix p = \case
  MTree x xs
    | p v -> (Nothing, Just 0)
    | p (v T.<<> xs) -> let !i = go 1 v xs in (Just (i-1), Just i)
    | otherwise -> let !i = T.size xs in (Just i, Nothing)
    where
      v = measure x
  MEmpty -> (Nothing, Nothing)
  where
    go !i !vup = \case
      MBin _ _ x l r
        | p v -> go i vup l
        | p v' -> i + T.size l
        | otherwise -> go (i + T.size l + 1) v' r
        where
          v = vup T.<<> l
          v' = v <> measure x
      MTip -> error "MSeq.binarySearchPrefix: bad p"
{-# INLINE binarySearchPrefix #-}

-- | \(O(\log n)\). Perform a binary search on the summaries of the non-empty
-- suffixes of the sequence.
--
-- @binarySearchSuffix p xs@ for a monotonic predicate @p@ returns two adjacent
-- indices @i@ and @j@, @0 <= i < j < length xs@.
--
-- * @i@ is the greatest index such that
--   @p (fromJust (summaryMay (drop i xs)))@ is
--   @True@, or @Nothing@ if there is no such index.
-- * @j@ is the least index such that
--   @p (fromJust (summaryMay (drop j xs)))@ is
--   @False@, or @Nothing@ if there is no such index
--
-- ==== __Examples__
--
-- @
-- import "Data.Monoid" (Sum(..))
--
-- newtype Elem = E Int deriving Show
--
-- instance Measured Elem where
--   type Measure Elem = Sum Int
--   measure (E x) = Sum x
-- @
--
-- >>> let xs = fromList [E 1, E 2, E 3, E 4]
--
-- The summaries of the suffixes of @xs@ by index are:
--
-- * @0: measure (E 1) <> measure (E 2) <> measure (E 3) <> measure (E 4) = Sum 10@.
-- * @1: measure (E 2) <> measure (E 3) <> measure (E 4) = Sum 9@.
-- * @2: measure (E 3) <> measure (E 4) = Sum 7@.
-- * @3: measure (E 4) = Sum 4@.
--
-- >>> binarySearchSuffix (> Sum 4) xs
-- (Just 2,Just 3)
--
-- @
--                  ╭──────────┬──────────┬──────────┬──────────╮
-- index:           │        0 │        1 │        2 │        3 │
--                  ├──────────┼──────────┼──────────┼──────────┤
-- suffix summary:  │   Sum 10 │    Sum 9 │    Sum 7 |    Sum 4 │
--                  ├──────────┼──────────┼──────────┼──────────┤
-- (> Sum 4):       │     True │     True │     True │    False │
--                  ╰──────────┴──────────┴──────────┴──────────╯
-- result:                                  ( Just 2 ,   Just 3 )
-- @
--
-- >>> binarySearchSuffix (> Sum 20) xs
-- (Nothing,Just 0)
--
-- @
--                           ╭──────────┬──────────┬──────────┬──────────╮
-- index:                    │        0 │        1 │        2 │        3 │
--                           ├──────────┼──────────┼──────────┼──────────┤
-- suffix summary:           │   Sum 10 │    Sum 9 │    Sum 7 |    Sum 4 │
--                           ├──────────┼──────────┼──────────┼──────────┤
-- (> Sum 20):               │    False │    False │    False │    False │
--                           ╰──────────┴──────────┴──────────┴──────────╯
-- result:         ( Nothing ,   Just 0 )
-- @
--
binarySearchSuffix
  :: Measured a => (Measure a -> Bool) -> MSeq a -> (Maybe Int, Maybe Int)
binarySearchSuffix p = \case
  MTree x xs -> case xs of
    MBin _ rv rx rl rr
      | p rv -> let !i = goR rx rl rr
                in if i == 0
                   then let !j = T.size xs in (Just j, Nothing)
                   else let !j = T.size xs - i in (Just j, Just (j+1))
      | p v -> (Just 0, Just 1)
      | otherwise -> (Nothing, Just 0)
      where
        v = measure x <> rv
    MTip
      | p (measure x) -> (Just 0, Nothing)
      | otherwise -> (Nothing, Just 0)
  MEmpty -> (Nothing, Nothing)
  where
    goR !x !l r = case r of
      MBin rsz rv rx rl rr
        | p rv -> goR rx rl rr
        | p v -> rsz
        | otherwise -> go (1 + rsz) v l
        where
          v = measure x <> rv
      MTip
        | p v -> 0
        | otherwise -> go 1 v l
        where
          v = measure x
    go !i !vup = \case
      MBin _ _ x l r
        | p v -> go i vup r
        | p v' -> i + T.size r
        | otherwise -> go (1 + T.size r + i) v' l
        where
          v = r T.<>> vup
          v' = measure x <> v
      MTip -> error "MSeq.binarySearchSuffix: bad p"
{-# INLINE binarySearchSuffix #-}

----------
-- Force
----------

-- | Reduce a sequence to normal form, given functions to reduce its contents.
liftRnf2 :: (Measure a -> ()) -> (a -> ()) -> MSeq a -> ()
liftRnf2 g f = \case
  MTree x xs -> f x `seq` T.liftRnf2 g f xs
  MEmpty -> ()
{-# INLINE liftRnf2 #-}

--------
-- Util
--------

fromMTree :: Measured a => MTree a -> MSeq a
fromMTree t = case T.uncons t of
  U.SNothing -> MEmpty
  U.SJust (U.S2 x xs) -> MTree x xs
{-# INLINE fromMTree #-}

-- See Note [compareLength]
compareLength :: MSeq a -> MSeq b -> Ordering
compareLength l r = case l of
  MTree _ xs -> case r of
    MTree _ ys -> compareSize xs ys
    MEmpty -> GT
  MEmpty -> case r of
    MTree _ _ -> LT
    MEmpty -> EQ
{-# INLINE compareLength #-}

compareSize :: MTree a -> MTree b -> Ordering
compareSize l r = case l of
  MBin szl _ _ _ _ -> case r of
    MBin szr _ _ _ _ -> compare szl szr
    MTip -> GT
  MTip -> case r of
    MBin _ _ _ _ _ -> LT
    MTip -> EQ
{-# INLINE compareSize #-}

----------
-- Build
----------

-- WARNING
--
-- The functions below are similar but they should not be mixed together! All of
-- them operate on Stack, but what the Stack means is not the same between
-- functions.
--
-- left-to-right, 1 element at a time: ltrPush, ltrFinish
-- left-to-right, many elements at a time: ltrPushMany, ltrFinish
-- right-to-left, 1 element at a time: rtlPush, rtlFinish

-- See Note [fromList implementation] in Data.Seqn.Internal.Seq
-- See Note [concatMap implementation] in Data.Seqn.Internal.Seq

ltrPush :: Measured a => Stack a -> a -> Stack a
ltrPush stk y = case stk of
  Push x MTip stk' -> ltrPushLoop stk' x 1 (T.singleton y)
  _ -> Push y MTip stk
{-# INLINABLE ltrPush #-}

ltrPushLoop :: Measured a => Stack a -> a -> Int -> MTree a -> Stack a
ltrPushLoop stk y !ysz ys = case stk of
  Push x xs@(MBin xsz _ _ _ _) stk'
    | xsz == ysz -> ltrPushLoop stk' x sz (T.binn sz y xs ys)
    where
      sz = xsz+xsz+1
  _ -> Push y ys stk
{-# INLINABLE ltrPushLoop #-}

rtlPush :: Measured a => a -> Stack a -> Stack a
rtlPush x = \case
  Push y MTip stk' -> rtlPushLoop x 1 (T.singleton y) stk'
  stk -> Push x MTip stk
{-# INLINABLE rtlPush #-}

rtlPushLoop :: Measured a => a -> Int -> MTree a -> Stack a -> Stack a
rtlPushLoop x !xsz xs = \case
  Push y ys@(MBin ysz _ _ _ _) stk'
    | xsz == ysz -> rtlPushLoop x sz (T.binn sz y xs ys) stk'
    where
      sz = xsz+xsz+1
  stk -> Push x xs stk
{-# INLINABLE rtlPushLoop #-}

ltrPushMany :: Measured a => Stack a -> a -> MTree a -> Stack a
ltrPushMany stk y ys = case stk of
  Push x xs stk'
    | ysz > xsz `div` 2 -> ltrPushManyLoop stk' x xsz xs y ysz ys
    | otherwise -> Push y ys stk
    where
      xsz = 1 + T.size xs
      ysz = 1 + T.size ys
  Nil -> Push y ys Nil
{-# INLINABLE ltrPushMany #-}

ltrPushManyLoop
  :: Measured a
  => Stack a -> a -> Int -> MTree a -> a -> Int -> MTree a -> Stack a
ltrPushManyLoop stk y !ysz ys z !zsz zs = case stk of
  Push x xs@(MBin xsz1 _ _ _ _) stk'
    | xsz < zsz
    -> ltrPushManyLoop stk' x (xsz + ysz) (T.link y xs ys) z zsz zs
    | yzsz > xsz `div` 2
    -> ltrPushManyLoop stk' x xsz xs y yzsz (T.link z ys zs)
    | otherwise
    -> Push y (T.link z ys zs) stk
    where
      xsz = 1+xsz1
      yzsz = ysz+zsz
  _ -> Push y (T.link z ys zs) stk
{-# INLINABLE ltrPushManyLoop #-}

ltrFinish :: Measured a => Stack a -> MSeq a
ltrFinish = wrapUpStack
  MEmpty
  U.S2
  (\(U.S2 y ys) x xs -> U.S2 x (T.link y xs ys))
  (\(U.S2 y ys) -> MTree y ys)
{-# INLINABLE ltrFinish #-}

rtlFinish :: Measured a => Stack a -> MSeq a
rtlFinish = wrapUpStack
  MEmpty
  U.S2
  (\(U.S2 x xs) y ys -> U.S2 x (T.link y xs ys))
  (\(U.S2 x xs) -> MTree x xs)
{-# INLINABLE rtlFinish #-}

-----------
-- Stream
-----------

-- See Note [Streams] in Data.Seqn.Internal.Seq

stream :: MSeq a -> Stream a
stream !t = Stream step s
  where
    s = case t of
      MTree x xs -> Push x xs Nil
      MEmpty -> Nil
    step = \case
      Nil -> Done
      Push x xs stk -> let !stk' = down xs stk in Yield x stk'
    {-# INLINE [0] step #-}
{-# INLINE stream #-}

streamEnd :: MSeq a -> Stream a
streamEnd !t = Stream step s
  where
    s = case t of
      MTree x xs -> Push x xs Nil
      MEmpty -> Nil
    step = \case
      Nil -> Done
      Push x xs stk -> case rDown x xs stk of
        U.S2 y stk' -> Yield y stk'
    {-# INLINE [0] step #-}
{-# INLINE streamEnd #-}

down :: MTree a -> Stack a -> Stack a
down (MBin _ _ x l r) stk = down l (Push x r stk)
down MTip stk = stk

rDown :: a -> MTree a -> Stack a -> U.S2 a (Stack a)
rDown !y (MBin _ _ x l r) stk = rDown x r (Push y l stk)
rDown y MTip stk = U.S2 y stk

----------
-- Stack
----------

-- This is used in various places. What it stores depends on the specific use
-- case.
data Stack a
  = Push !a !(MTree a) !(Stack a)
  | Nil

wrapUpStack
  :: c -- empty
  -> (a -> MTree a -> b) -- initial
  -> (b -> a -> MTree a -> b) -- fold fun
  -> (b -> c) -- finish
  -> Stack a
  -> c
wrapUpStack z0 f0 f fin = go
  where
    go Nil = z0
    go (Push x xs stk) = go1 (f0 x xs) stk
    go1 !z Nil = fin z
    go1 z (Push x xs stk) = go1 (f z x xs) stk
{-# INLINE wrapUpStack #-}

------------
-- Testing
------------

valid :: (Measured a, Eq (Measure a)) => MSeq a -> Bool
valid = \case
  MTree _ xs -> T.valid xs
  MEmpty -> True

debugShowsPrec :: (Show a, Show (Measure a)) => Int -> MSeq a -> ShowS
debugShowsPrec p = \case
  MTree x xs ->
    showParen (p > 10) $
      showString "MTree " .
      showsPrec 11 x .
      showString " " .
      T.debugShowsPrec 11 xs
  MEmpty -> showString "MEmpty"

----------
-- Error
----------

errorElement :: a
errorElement = error "MSeq: errorElement"
