{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- This is an internal module. You probably don't need to import this. Use
-- "Data.Seqn.Seq" instead.
--
-- = WARNING
--
-- Definitions in this module allow violating invariants that would otherwise be
-- guaranteed by "Data.Seqn.Seq". Use at your own risk!
--
module Data.Seqn.Internal.Seq
  (
    -- * Seq
    Seq(..)

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
  , tails
  , inits
  , chunksOf

    -- * Filter
  , filter
  , catMaybes
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
  , zip
  , zip3
  , zipWith
  , zipWith3
  , zipWithM
  , zipWith3M
  , unzip
  , unzip3
  , unzipWith
  , unzipWith3

    -- * Internal
  , fromTree

    -- * Testing
  , valid
  , debugShowsPrec
  ) where

import Prelude hiding (concatMap, break, drop, dropWhile, filter, lookup, replicate, reverse, scanl, scanr, span, splitAt, take, takeWhile, traverse, unzip, unzip3, zip, zip3, zipWith, zipWith3)
import qualified Control.Applicative as Ap
import Control.Applicative.Backwards (Backwards(..))
import Control.DeepSeq (NFData(..), NFData1(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Zip (MonadZip(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Coerce (coerce)
import qualified Data.Foldable as F
import qualified Data.Foldable.WithIndex as IFo
import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), Read1(..))
import qualified Data.Functor.Classes as F1
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import qualified Data.Functor.WithIndex as IFu
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Monoid as Monoid
import qualified Data.Primitive.Array as A
import qualified Data.SamSort as Sam
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import qualified Data.Traversable as Tr
import qualified Data.Traversable.WithIndex as ITr
import qualified GHC.Exts as X
import Text.Read (Read(..))
import qualified Text.Read as Read

import qualified Data.Seqn.Internal.KMP as KMP
import Data.Seqn.Internal.Stream (Stream(..), Step(..))
import qualified Data.Seqn.Internal.Stream as Stream
import Data.Seqn.Internal.Tree (Tree(..))
import qualified Data.Seqn.Internal.Tree as T
import qualified Data.Seqn.Internal.Util as U

--------
-- Seq
--------

-- | A sequence with elements of type @a@.
data Seq a
  = Tree !a !(Tree a)
  | Empty

-- Note [Seq structure]
-- ~~~~~~~~~~~~~~~~~~~~
-- A Seq is a weight-balanced binary tree, with a small twist: the first element
-- is kept aside from the tree. It can be viewed as a binary tree with a root
-- and a right child, but a missing left child. The motivation for this change
-- is that it improves the complexity of the append operation, from
-- O(log (n_1 + n_2)) to O(|log n_1 - log n_2|), while not affecting any of the
-- other operations. Is it worth the trouble? I think so.

--------------
-- Instances
--------------

instance Eq a => Eq (Seq a) where
  t1 == t2 = compareLength t1 t2 == EQ && stream t1 == stream t2
  {-# INLINABLE (==) #-}

-- | Lexicographical ordering
instance Ord a => Ord (Seq a) where
  compare t1 t2 = compare (stream t1) (stream t2)
  {-# INLINABLE compare #-}

instance Show a => Show (Seq a) where
  showsPrec _ t = shows (F.toList t)
  {-# INLINABLE showsPrec #-}

instance Read a => Read (Seq a) where
  readPrec = fmap fromList readListPrec
  {-# INLINABLE readPrec #-}

  readListPrec = Read.readListPrecDefault
  {-# INLINABLE readListPrec #-}

instance Eq1 Seq where
  liftEq f t1 t2 = compareLength t1 t2 == EQ && liftEq f (stream t1) (stream t2)
  {-# INLINE liftEq #-}

instance Ord1 Seq where
  liftCompare f t1 t2 = liftCompare f (stream t1) (stream t2)
  {-# INLINE liftCompare #-}

instance Show1 Seq where
  liftShowsPrec _ sl _ t = sl (F.toList t)
  {-# INLINE liftShowsPrec #-}

instance Read1 Seq where
  liftReadPrec _ = fmap fromList
  liftReadListPrec = F1.liftReadListPrecDefault

-- |
-- [@length@]: \(O(1)\).
--
-- Folds are \(O(n)\).
instance Foldable Seq where
  fold = foldMap id
  {-# INLINABLE fold #-}

  foldMap f = Tr.foldMapDefault f
  {-# INLINE foldMap #-}

  foldMap' f = F.foldl' (\z x -> z <> f x) mempty
  {-# INLINE foldMap' #-}

  foldr f z = Stream.foldr f z . stream
  {-# INLINE foldr #-}

  foldl f z = Stream.foldr (flip f) z . streamEnd
  {-# INLINE foldl #-}

  foldl' f !z = \case
    Tree x xs -> T.foldl' f (f z x) xs
    Empty -> z
  {-# INLINE foldl' #-}

  foldr' f !z = \case
    Tree x xs -> f x $! T.foldr' f z xs
    Empty -> z
  {-# INLINE foldr' #-}

  null = \case
    Tree _ _ -> False
    Empty -> True

  length = \case
    Tree _ xs -> 1 + T.size xs
    Empty -> 0

-- |
-- [@fmap@]: \(O(n)\).
--
-- [@(<$)@]: \(O(\log n)\).
instance Functor Seq where
  fmap f = Tr.fmapDefault f
  {-# INLINE fmap #-}

  x <$ xs = replicate (length xs) x

instance Traversable Seq where
  traverse f = \case
    Empty -> pure Empty
    Tree x xs -> Ap.liftA2 Tree (f x) (T.traverse f xs)
  {-# INLINE traverse #-}

-- |
-- [@(<>)@]: \(O(\left| \log n_1 - \log n_2 \right|)\). Concatenates two
-- sequences.
--
-- [@stimes@]: \(O(\log c)\). @stimes c xs@ is @xs@ repeated @c@ times. If
-- @c < 0@, 'empty' is returned.
instance Semigroup (Seq a) where
  Tree x xs <> Tree y ys = Tree x (T.link y xs ys)
  l <> Empty = l
  Empty <> r = r

  stimes !c = \case
    t@(Tree x xs)
      | c <= 0 -> Empty
      | fromIntegral c * toi (length t) > toi (maxBound :: Int) ->
          error "Seq.stimes: result size too large"
      | otherwise -> stimesGo x (c'-1) xs xs
      where
        c' = fromIntegral c :: Int
        toi :: Int -> Integer
        toi = fromIntegral
    Empty -> Empty
  {-# INLINABLE stimes #-}

  sconcat (x:|xs) = mconcat (x:xs)

-- Note [Complexity of stimes]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Let stimesGo be initially called with trees (xs and acc) of size (n-1).
--
-- go is called O(log c) times in total, since c halves on every call.
-- At any iteration, xs is made up of initial tree bin-ed with itself multiple
-- times, and acc is made up of the some of the xs linked together.
-- All operations in go are O(1) except for link, which takes
-- O(log(size xs) - log(size acc)).
--
-- The cost of the ith iteration is O(1) if 2^i is in c.
-- If not, a link is done with some cost depending on xs and acc.
-- For iteration i, the size of xs is 2^i n - 1.
-- The size of acc is (sum of 2^p_j * n) - 1 for the powers of 2 p_j < i in c.
-- Let there be k powers of 2 in c, i.e. c = \sum_{i=1}^k p_i.
-- Then the total cost of the links is
--   O(\sum_{i=2}^k (\log (2^{p_i} n - 1) - \log ((\sum_{j=1}^{i-1} 2^{p_j} n) - 1))))
-- = O(\sum_{i=2}^k (\log (2^{p_i} n)     - \log (\sum_{j=1}^{i-1} 2^{p_j} n)))
-- = O(\sum_{i=2}^k (\log (2^{p_i} n)     - \log (2^{p_{i-1}} n)))
-- = O(\sum_{i=2}^k (p_i + \log n         - p_{i-1} - \log n))
-- = O(\sum_{i=2}^k (p_i - p_{i-1}))
-- = O(p_k - p_1)
-- = O(\log c)

stimesGo :: a -> Int -> Tree a -> Tree a -> Seq a
stimesGo !x = go
  where
    go c !xs !acc
      | c <= 0 = Tree x acc
      | c `mod` 2 == 0 = go (c `div` 2) (T.bin x xs xs) acc
      | otherwise = go (c `div` 2) (T.bin x xs xs) (T.link x xs acc)
{-# INLINE stimesGo #-}

-- |
-- [@mempty@]: The empty sequence.
instance Monoid (Seq a) where
  mempty = Empty

  mconcat = concatMap id
  {-# INLINE mconcat #-} -- Inline for fusion

instance NFData a => NFData (Seq a) where
  rnf = \case
    Tree x xs -> rnf x `seq` rnf xs
    Empty -> ()
  {-# INLINABLE rnf #-}

instance NFData1 Seq where
  liftRnf f = \case
    Tree x xs -> f x `seq` liftRnf f xs
    Empty -> ()
  {-# INLINE liftRnf #-}

-- |
-- [@liftA2@]: \(O(n_1 n_2)\).
--
-- [@(<*)@]: \(O(n_1 \log n_2)\).
--
-- [@(*>)@]: \(O(\log n_1)\).
instance Applicative Seq where
  pure = singleton

  liftA2 f t1 t2 = case t2 of
    Empty -> Empty
    Tree x Tip -> fmap (`f` x) t1
    _ -> concatMap (\x -> fmap (f x) t2) t1
  {-# INLINE liftA2 #-}

  t1 <* t2 = case t2 of
    Empty -> Empty
    Tree _ Tip -> t1
    _ -> concatMap (replicate (length t2)) t1

  s1 *> s2 = stimes (length s1) s2

instance Ap.Alternative Seq where
  empty = Empty
  (<|>) = (<>)

instance Monad Seq where
  t >>= f = concatMap f t
  {-# INLINE (>>=) #-}

instance MonadPlus Seq

instance MonadFail Seq where
  fail _ = Empty

instance MonadFix Seq where
  mfix f =
    IFu.imap
      (\i _ -> let x = index i (f x) in x)
      (f (error "Seq.mfix: f must be lazy"))
  {-# INLINE mfix #-}

instance MonadZip Seq where
  mzip = zip
  mzipWith = zipWith
  munzip = unzip

instance (a ~ Char) => IsString (Seq a) where
  fromString = fromList

instance X.IsList (Seq a) where
  type Item (Seq a) = a
  fromList = fromList
  {-# INLINE fromList #-}

  toList = F.toList
  {-# INLINE toList #-}

instance IFu.FunctorWithIndex Int Seq where
  imap f = ITr.imapDefault f
  {-# INLINE imap #-}

instance IFo.FoldableWithIndex Int Seq where
  ifoldMap f = ITr.ifoldMapDefault f
  {-# INLINE ifoldMap #-}

  ifoldr f z = Stream.ifoldr f z 0 (+1) . stream
  {-# INLINE ifoldr #-}

  ifoldl f z = \t ->
    Stream.ifoldr (flip . f) z (length t - 1) (subtract 1) (streamEnd t)
  {-# INLINE ifoldl #-}

  ifoldr' f !z = \case
    Tree x xs -> f 0 x $! T.ifoldr' f z (T.size xs) xs
    Empty -> z
  {-# INLINE ifoldr' #-}

  ifoldl' f !z = \case
    Tree x xs -> T.ifoldl' f (f 0 z x) 1 xs
    Empty -> z
  {-# INLINE ifoldl' #-}

instance ITr.TraversableWithIndex Int Seq where
  itraverse f = \case
    Empty -> pure Empty
    Tree x xs -> Ap.liftA2 Tree (f 0 x) (T.itraverse f 1 xs)
  {-# INLINE itraverse #-}

--------------
-- Construct
--------------

-- | The empty sequence.
empty :: Seq a
empty = Empty

-- | A singleton sequence.
singleton :: a -> Seq a
singleton x = Tree x Tip

-- | \(O(n)\). Create a @Seq@ from a list.
--
-- ==== __Examples__
--
-- >>> fromList [8,1,19,11,5,12,12]
-- [8,1,19,11,5,12,12]
fromList :: [a] -> Seq a
fromList = ltrFinish . F.foldl' ltrPush Nil
{-# INLINE fromList #-}
-- See Note [fromList implementation]

-- | \(O(n)\). Create a @Seq@ from a reversed list.
--
-- ==== __Examples__
--
-- >>> fromRevList "!olleH"
-- "Hello!"
fromRevList :: [a] -> Seq a
fromRevList = rtlFinish . F.foldl' (flip rtlPush) Nil
{-# INLINE fromRevList #-}
-- See Note [fromList implementation]

-- | \(O(\log n)\). A sequence with a repeated element.
-- If the length is negative, 'empty' is returned.
--
-- ==== __Examples__
--
-- >>> replicate 3 "ha"
-- ["ha","ha","ha"]
replicate :: Int -> a -> Seq a
replicate n !x
  | n <= 0 = Empty
  | otherwise = stimesGo x (n-1) Tip Tip

-- | \(O(n)\). Generate a sequence from a length and an applicative action.
-- If the length is negative, 'empty' is returned.
--
-- ==== __Examples__
--
-- >>> import System.Random (randomIO)
-- >>> import Data.Word (Word8)
-- >>> replicateA 5 (randomIO :: IO Word8)
-- [26,134,30,58,221]
replicateA :: Applicative f => Int -> f a -> f (Seq a)
replicateA !n m = generateA n (const m)
{-# INLINABLE replicateA #-}

-- | \(O(n)\). Generate a sequence from a length and a generator.
-- If the length is negative, 'empty' is returned.
--
-- ==== __Examples__
--
-- >>> generate 4 (10*)
-- [0,10,20,30]
generate :: Int -> (Int -> a) -> Seq a
generate =
  (coerce :: (Int -> (Int -> Identity a) -> Identity (Seq a))
          -> Int -> (Int -> a) -> Seq a)
  generateA
{-# INLINE generate #-}

-- | \(O(n)\). Generate a sequence from a length and an applicative generator.
-- If the length is negative, 'empty' is returned.
generateA :: Applicative f => Int -> (Int -> f a) -> f (Seq a)
generateA n f
  | n <= 0 = pure Empty
  | otherwise = Ap.liftA2 Tree (f 0) (T.generateA f 1 (n-1))
{-# INLINE generateA #-}

-- | \(O(n)\). Unfold a sequence from left to right.
--
-- ==== __Examples__
--
-- >>> let f (i,a,b) = if i >= 10 then Nothing else Just (a, (i+1, b, a+b))
-- >>> unfoldr f (0,0,1)
-- [0,1,1,2,3,5,8,13,21,34]
unfoldr :: (b -> Maybe (a, b)) -> b -> Seq a
unfoldr =
  (coerce :: ((b -> Identity (Maybe (a, b))) -> b -> Identity (Seq a))
          -> (b -> Maybe (a, b)) -> b -> Seq a)
  unfoldrM
{-# INLINE unfoldr #-}

-- | \(O(n)\). Unfold a sequence monadically from left to right.
unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m (Seq a)
unfoldrM f = go Nil
  where
    go !b z = f z >>= \case
      Nothing -> pure $! ltrFinish b
      Just (x, z') -> go (ltrPush b x) z'
{-# INLINE unfoldrM #-}

-- | \(O(n)\). Unfold a sequence from right to left.
--
-- ==== __Examples__
--
-- >>> let f i = if i <= 0 then Nothing else Just (i `div` 2, i)
-- >>> unfoldl f 1024
-- [1,2,4,8,16,32,64,128,256,512,1024]
unfoldl :: (b -> Maybe (b, a)) -> b -> Seq a
unfoldl =
  (coerce :: ((b -> Identity (Maybe (b, a))) -> b -> Identity (Seq a))
          -> (b -> Maybe (b, a)) -> b -> Seq a)
  unfoldlM
{-# INLINE unfoldl #-}

-- | \(O(n)\). Unfold a sequence monadically from right to left.
unfoldlM :: Monad m => (b -> m (Maybe (b, a))) -> b -> m (Seq a)
unfoldlM f = go Nil
  where
    go !b z = f z >>= \case
      Nothing -> pure $! rtlFinish b
      Just (z', x) -> go (rtlPush x b) z'
{-# INLINE unfoldlM #-}

-- | \(O \left(\sum_i \log n_i \right)\).
-- Map over a @Foldable@ and concatenate the results.
--
-- ==== __Examples__
--
-- >>> concatMap (uncurry replicate) [(1,'H'),(1,'e'),(2,'l'),(1,'o')]
-- "Hello"
concatMap :: Foldable f => (a -> Seq b) -> f a -> Seq b
concatMap f = ltrFinish . F.foldl' g Nil
  where
    g b x = case f x of
      Empty -> b
      Tree y ys -> ltrPushMany b y ys
    {-# INLINE g #-}
{-# INLINE concatMap #-}
-- See Note [concatMap implementation]

------------
-- Convert
------------

-- | \(O(n)\). Convert to a list in reverse.
--
-- To convert to a list without reversing, use
-- @Data.Foldable.'Data.Foldable.toList'@.
--
-- ==== __Examples__
--
-- >>> toRevList (fromList "!olleH")
-- "Hello!"
toRevList :: Seq a -> [a]
toRevList t = X.build $ \lcons lnil -> F.foldl (flip lcons) lnil t
{-# INLINE toRevList #-}

----------
-- Index
----------

-- Precondition: 0 <= i < size xs
indexTree :: Int -> Tree a -> a
indexTree !i xs = getConst (T.adjustF Const i xs)

-- | \(O(\log n)\). Look up the element at an index.
--
-- ==== __Examples__
--
-- >>> lookup 3 (fromList "haskell")
-- Just 'k'
-- >>> lookup (-1) (singleton 7)
-- Nothing
lookup :: Int -> Seq a -> Maybe a
lookup !i (Tree x xs)
  | i < 0 || T.size xs < i = Nothing
  | i == 0 = Just x
  | otherwise = Just $! indexTree (i-1) xs
lookup _ Empty = Nothing
{-# INLINE lookup #-}

-- | \(O(\log n)\). Look up the element at an index. Calls @error@ if the index
-- is out of bounds.
--
-- ==== __Examples__
--
-- >>> index 3 (fromList "haskell")
-- 'k'
-- >>> index (-1) (singleton 7)
-- *** Exception: ...
index :: Int -> Seq a -> a
index !i = \case
  Tree x xs
    | i == 0 -> x
    | otherwise -> indexTree (i-1) xs
  Empty -> error "Seq.index: out of bounds"

-- | \(O(\log n)\). Infix version of 'lookup'.
(!?) :: Seq a -> Int -> Maybe a
(!?) = flip lookup
{-# INLINE (!?) #-}

-- | \(O(\log n)\). Infix version of 'index'. Calls @error@ if the index is out
-- of bounds.
(!) :: Seq a -> Int -> a
(!) = flip index

-- | \(O(\log n)\). Update an element at an index. If the index is out of
-- bounds, the sequence is returned unchanged.
--
-- ==== __Examples__
--
-- >>> update 3 'b' (fromList "bird")
-- "birb"
-- >>> update 3 True (singleton False)
-- [False]
update :: Int -> a -> Seq a -> Seq a
update i x t = adjust (const x) i t

-- | \(O(\log n)\). Adjust the element at an index. If the index is out of
-- bounds the sequence is returned unchanged.
--
-- ==== __Examples__
--
-- >>> adjust Data.List.reverse 1 (fromList ["Hello", "ereht"])
-- ["Hello","there"]
-- >>> adjust (*100) (-1) (singleton 7)
-- [7]
adjust :: (a -> a) -> Int -> Seq a -> Seq a
adjust f !i t = case t of
  Tree x xs
    | i < 0 || T.size xs < i -> t
    | i == 0 -> Tree (f x) xs
    | otherwise -> Tree x (runIdentity (T.adjustF (Identity U.#. f) (i-1) xs))
  Empty -> Empty
{-# INLINE adjust #-}

-- | \(O(\log n)\). Insert an element at an index. If the index is out of
-- bounds, the element is added to the closest end of the sequence.
--
-- ==== __Examples__
--
-- >>> insertAt 1 'a' (fromList "ct")
-- "cat"
-- >>> insertAt (-10) 0 (fromList [5,6,7])
-- [0,5,6,7]
-- >>> insertAt 10 0 (fromList [5,6,7])
-- [5,6,7,0]
insertAt :: Int -> a -> Seq a -> Seq a
insertAt !i y t = case t of
  Tree x xs
    | i <= 0 -> cons y t
    | otherwise -> Tree x (T.insertAt (i-1) y xs)
  Empty -> singleton y

-- | \(O(\log n)\). Delete an element at an index. If the index is out of
-- bounds, the sequence is returned unchanged.
--
-- ==== __Examples__
--
-- >>> deleteAt 2 (fromList "cart")
-- "cat"
-- >>> deleteAt 10 (fromList [5,6,7])
-- [5,6,7]
deleteAt :: Int -> Seq a -> Seq a
deleteAt !i t = case t of
  Tree x xs
    | i < 0 || T.size xs < i -> t
    | i == 0 -> fromTree xs
    | otherwise -> Tree x (T.deleteAt (i-1) xs)
  Empty -> Empty

------------
-- Slicing
------------

-- | \(O(\log n)\). Append a value to the beginning of a sequence.
--
-- ==== __Examples__
--
-- >>> cons 1 (fromList [2,3])
-- [1,2,3]
cons :: a -> Seq a -> Seq a
cons x (Tree y ys) = Tree x (T.cons y ys)
cons x Empty = singleton x

-- | \(O(\log n)\). Append a value to the end of a sequence.
--
-- ==== __Examples__
--
-- >>> snoc (fromList [1,2]) 3
-- [1,2,3]
snoc :: Seq a -> a -> Seq a
snoc (Tree y ys) x = Tree y (T.snoc ys x)
snoc Empty x = singleton x

-- | \(O(\log n)\). The head and tail of a sequence.
--
-- ==== __Examples__
--
-- >>> uncons (fromList [1,2,3])
-- Just (1,[2,3])
-- >>> uncons empty
-- Nothing
uncons :: Seq a -> Maybe (a, Seq a)
uncons (Tree x xs) = Just . (,) x $! fromTree xs
uncons Empty = Nothing
{-# INLINE uncons #-}

-- | \(O(\log n)\). The init and last of a sequence.
--
-- ==== __Examples__
--
-- >>> unsnoc (fromList [1,2,3])
-- Just ([1,2],3)
-- >>> unsnoc empty
-- Nothing
unsnoc :: Seq a -> Maybe (Seq a, a)
unsnoc (Tree x xs) = case T.unsnoc xs of
  U.SNothing -> Just (Empty, x)
  U.SJust (U.S2 ys y) -> Just (Tree x ys, y)
unsnoc Empty = Nothing
{-# INLINE unsnoc #-}

-- | \(O(\log n)\). Take a number of elements from the beginning of a sequence.
--
-- ==== __Examples__
--
-- >>> take 3 (fromList "haskell")
-- "has"
-- >>> take (-1) (fromList [1,2,3])
-- []
-- >>> take 10 (fromList [1,2,3])
-- [1,2,3]
take :: Int -> Seq a -> Seq a
take !i t@(Tree x xs)
  | i <= 0 = Empty
  | T.size xs < i = t
  | otherwise = Tree x (getConst (T.splitAtF (i-1) xs))
take _ Empty = Empty

-- | \(O(\log n)\). Drop a number of elements from the beginning of a sequence.
--
-- ==== __Examples__
--
-- >>> drop 3 (fromList "haskell")
-- "kell"
-- >>> drop (-1) (fromList [1,2,3])
-- [1,2,3]
-- >>> drop 10 (fromList [1,2,3])
-- []
drop :: Int -> Seq a -> Seq a
drop !i t@(Tree _ xs)
  | i <= 0 = t
  | T.size xs < i = Empty
  | otherwise = case U.unTagged (T.splitAtF (i-1) xs) of
      U.S2 x' xs' -> Tree x' xs'
drop _ Empty = Empty

-- | \(O(\log n)\). Take a number of elements from the end of a sequence.
takeEnd :: Int -> Seq a -> Seq a
takeEnd n t = drop (length t - n) t

-- | \(O(\log n)\). Drop a number of elements from the end of a sequence.
dropEnd :: Int -> Seq a -> Seq a
dropEnd n t = take (length t - n) t

-- | \(O(\log n)\). The slice of a sequence between two indices (inclusive).
--
-- ==== __Examples__
--
-- >>> slice (1,3) (fromList "haskell")
-- "ask"
-- >>> slice (-10,2) (fromList [1,2,3,4,5])
-- [1,2,3]
-- >>> slice (2,1) (fromList [1,2,3,4,5])
-- []
slice :: (Int, Int) -> Seq a -> Seq a
slice (i,j) = drop i . take (j+1)

-- | \(O(\log n)\). Split a sequence at a given index.
--
-- @splitAt n xs == ('take' n xs, 'drop' n xs)@
--
-- ==== __Examples__
--
-- >>> splitAt 3 (fromList "haskell")
-- ("has","kell")
-- >>> splitAt (-1) (fromList [1,2,3])
-- ([],[1,2,3])
-- >>> splitAt 10 (fromList [1,2,3])
-- ([1,2,3],[])
splitAt :: Int -> Seq a -> (Seq a, Seq a)
splitAt !i t@(Tree x xs)
  | i <= 0 = (Empty, t)
  | T.size xs < i = (t, Empty)
  | otherwise = case T.splitAtF (i-1) xs of
      U.S2 xs1 (U.S2 x' xs2) -> (Tree x xs1, Tree x' xs2)
splitAt _ Empty = (Empty, Empty)

-- | \(O(\log n)\). Split a sequence at a given index from the end.
--
-- @splitAtEnd n xs == ('dropEnd' n xs, 'takeEnd' n xs)@
splitAtEnd :: Int -> Seq a -> (Seq a, Seq a)
splitAtEnd i s = splitAt (length s - i) s

-- | \(O(n \log n)\). All suffixes of a sequence, longest first.
--
-- ==== __Examples__
--
-- >>> tails (fromList [1,2,3])
-- [[1,2,3],[2,3],[3],[]]
tails :: Seq a -> Seq (Seq a)
tails t0 = cons t0 (U.evalSState (Tr.traverse f t0) t0)
  where
    f _ = U.sState $ \t -> case uncons t of
      Nothing -> U.S2 t t -- impossible
      -- Could have been error but https://gitlab.haskell.org/ghc/ghc/-/issues/24806
      Just (_,t') -> U.S2 t' t'
-- See Note [Tails implementation]

-- | \(O(n \log n)\). All prefixes of a sequence, shortest first.
--
-- ==== __Examples__
--
-- >>> inits (fromList [1,2,3])
-- [[],[1],[1,2],[1,2,3]]
inits :: Seq a -> Seq (Seq a)
inits t0 = snoc (U.evalSState (forwards (Tr.traverse f t0)) t0) t0
  where
    f _ = Backwards $ U.sState $ \t -> case unsnoc t of
      Nothing -> U.S2 t t -- impossible
      Just (t',_) -> U.S2 t' t'

-- Note [Tails implementation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- tails :: Seq a -> Seq (Seq a)
--
-- There are many ways to implement tails (and inits), with different
-- <WHNF, WHNF for ith tail>:
--
-- 1. (Generate or imap) with drop                 : <O(n), O(log n)>
-- 2. Send down a stack and rebuild                : <O(n), O(log n)>
-- 3. (Unfold, replicateA or traverse) with uncons : <O(n log n), O(n log n)>
--
-- We do 3 for because it is faster in benchmarks. It cannot be done lazily,
-- unlike 1 and 2, but that is fine because Seq is value-strict.


-- | \(O \left(\frac{n}{c} \log c \right)\). Split a sequence into chunks of the
-- given length @c@. If @c <= 0@, 'empty' is returned.
--
-- ==== __Examples__
--
-- >>> chunksOf 3 (fromList [1..10])
-- [[1,2,3],[4,5,6],[7,8,9],[10]]
-- >>> chunksOf 10 (fromList "hello")
-- ["hello"]
-- >>> chunksOf (-1) (singleton 7)
-- []

-- See Note [chunksOf complexity]
chunksOf :: Int -> Seq a -> Seq (Seq a)
chunksOf !c t@(Tree x xs)
  | c <= 0 = Empty
  | c == 1 = fmap singleton t
  | length t <= c = singleton t
  | otherwise = case chunksOf_ c 1 xs of
      U.S3 l m r -> case r of
        T.Tip -> Tree (Tree x l) m
        _ -> Tree (Tree x l) (T.snoc m (fromTree r))
chunksOf _ Empty = Empty

-- Preconditions:
-- 1. c > 1
-- 2. at least one chunk boundary passes through the tree
chunksOf_ :: Int -> Int -> Tree a -> U.S3 (Tree a) (Tree (Seq a)) (Tree a)
chunksOf_ !_ !_ Tip = error "Seq.chunksOf_: precondition violated"
chunksOf_ c off (Bin sz x l r) = case (lHasSplit, rHasSplit) of
  (False, False) ->
    -- Here exactly one of (lend==c) and (roff==0) is true.
    -- If both are true, precondition 1 was violated.
    -- If both are false, precondition 2 was violated.
    -- We check roff==0 and assume the other is the complement.
    case (off==0, roff==0, rend==c) of
      (False, True , False) -> U.S3 (T.snoc l x) T.Tip r
      (False, True , True ) -> U.S3 (T.snoc l x) (t1 (fromTree r)) T.Tip
      (False, False, False) -> U.S3 l T.Tip (T.cons x r)
      (False, False, True ) -> U.S3 l (t1 (Tree x r)) T.Tip
      (True , False, False) -> U.S3 T.Tip (t1 (fromTree l)) (T.cons x r)
      (True , False, True ) -> U.S3 T.Tip (t2 (fromTree l) (Tree x r)) T.Tip
      (True , True , False) -> U.S3 T.Tip (t1 (fromTree (T.snoc l x))) r
      (True , True , True ) ->
        U.S3 T.Tip (t2 (fromTree (T.snoc l x)) (fromTree r)) T.Tip
  (False, True) -> case chunksOf_ c roff r of
    U.S3 rl rm rr -> case (off==0, lend==c) of
      (False, False) -> U.S3 (T.link x l rl) rm rr
      (False, True ) -> U.S3 l (T.cons (Tree x rl) rm) rr
      (True , False) -> U.S3 T.Tip (T.cons (fromTree (T.link x l rl)) rm) rr
      (True , True ) ->
        U.S3 T.Tip (T.cons (fromTree l) (T.cons (Tree x rl) rm)) rr
  (True, False) -> case chunksOf_ c off l of
    U.S3 ll lm lr -> case (roff==0, rend==c) of
      (False, False) -> U.S3 ll lm (T.link x lr r)
      (False, True ) -> U.S3 ll (T.snoc lm (fromTree (T.link x lr r))) T.Tip
      (True , False) -> U.S3 ll (T.snoc lm (fromTree (T.snoc lr x))) r
      (True , True ) ->
        U.S3 ll
              (T.snoc (T.snoc lm (fromTree (T.snoc lr x))) (fromTree r))
              T.Tip
  (True, True) -> case (chunksOf_ c off l, chunksOf_ c roff r) of
    (U.S3 ll lm lr, U.S3 rl rm rr) ->
      U.S3 ll (T.link (fromTree (T.link x lr rl)) lm rm) rr
  where
    szl = T.size l
    szr = sz - szl - 1
    lend = off + szl
    roff = (lend + 1) `rem` c
    rend = roff + szr
    lHasSplit = lend > c
    rHasSplit = rend > c
    t1 y = T.Bin 1 y T.Tip T.Tip
    t2 y1 y2 = T.Bin 2 y1 T.Tip (T.Bin 1 y2 T.Tip T.Tip)

-- Note [chunksOf complexity]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The tree of size n is partitioned into ceil(n/c) chunks, each of size at
-- most c. Each chunk is a contiguous subsequence of the original tree, and
-- such chunks are balanced in O(log c) time. The finalizing step of such a
-- chunk is a link of some left and right trees with a root. Since they can be
-- of any size, bounded by the total c, this is O(log c). The left tree here
-- is the result of multiple links of (root, right child) caused by the split
-- at a chunk boundary, again with total size bounded by c. This takes
-- O(log c). For an explanation see the description of the finishing step in
-- Note [fromList complexity]. The same applies to the right tree. Hence, each
-- chunk is balanced in O(log c) and to balance all the chunks we need
-- O((n/c) log c).
--
-- Now the result tree has size ceil(n/c), which needs to be balanced. This is
-- done by linking the recursive results from the left and right children, l and
-- r. The results are triples of
-- (left incomplete chunk, complete chunks, right incomplete chunk).
-- The number of complete chunks from the left child, say l', is at least
-- lmin=ceil((lsz-2(c-1))/c) and at most lmax=floor(lsz/c). It is likewise for
-- the right child, which returns rmin<=r'<=rmax chunks. Balancing the linked
-- tree takes O(|log(l'sz) - log(r'sz)|)
-- = O(max(log lmax - log rmin, log rmax - log lmin))
-- = O(max(log(lmax/rmin), log(rmax/lmin)))
-- = O(max(log(lsz/rsz), log(rsz/lsz))   ; lmax is Θ(lsz/c), rmax is Θ(rsz/c)
-- = O(1)                                ; lsz<=3*rsz && rsz<=3*lsz by balance
-- So all the balancing work here is done in O(n/c).
--
-- The total is dominated by balancing all the chunks, giving us O((n/c) log c).

--------------
-- Filtering
--------------

-- | \(O(n)\). Keep elements that satisfy a predicate.
--
-- ==== __Examples__
--
-- >>> filter even (fromList [1..10])
-- [2,4,6,8,10]
filter :: (a -> Bool) -> Seq a -> Seq a
filter =
  (coerce :: ((a -> Identity Bool) -> Seq a -> Identity (Seq a))
          -> (a -> Bool) -> Seq a -> Seq a)
  filterA
{-# INLINE filter #-}

-- | \(O(n)\). Keep the @Just@s in a sequence.
--
-- ==== __Examples__
--
-- >>> catMaybes (fromList [Just 1, Nothing, Nothing, Just 10, Just 100])
-- [1,10,100]
catMaybes :: Seq (Maybe a) -> Seq a
catMaybes t = mapMaybe id t

-- | \(O(n)\). Map over elements and collect the @Just@s.
mapMaybe :: (a -> Maybe b) -> Seq a -> Seq b
mapMaybe =
  (coerce :: ((a -> Identity (Maybe b)) -> Seq a -> Identity (Seq b))
          -> (a -> Maybe b) -> Seq a -> Seq b)
  mapMaybeA
{-# INLINE mapMaybe #-}

-- | \(O(n)\). Map over elements and split the @Left@s and @Right@s.
--
-- ==== __Examples__
--
-- >>> mapEither (\x -> if odd x then Left x else Right x) (fromList [1..10])
-- ([1,3,5,7,9],[2,4,6,8,10])
mapEither :: (a -> Either b c) -> Seq a -> (Seq b, Seq c)
mapEither =
  (coerce :: ((a -> Identity (Either b c)) -> Seq a -> Identity (Seq b, Seq c))
          -> (a -> Either b c) -> Seq a -> (Seq b, Seq c))
  mapEitherA
{-# INLINE mapEither #-}

-- | \(O(n)\). Keep elements that satisfy an applicative predicate.
filterA :: Applicative f => (a -> f Bool) -> Seq a -> f (Seq a)
filterA f = mapMaybeA (\x -> fmap (\b -> if b then Just x else Nothing) (f x))
{-# INLINE filterA #-}

-- | \(O(n)\). Traverse over elements and collect the @Just@s.
mapMaybeA :: Applicative f => (a -> f (Maybe b)) -> Seq a -> f (Seq b)
mapMaybeA f = \case
  Tree x xs -> Ap.liftA2 (maybe fromTree Tree) (f x) (T.mapMaybeA f xs)
  Empty -> pure Empty
{-# INLINE mapMaybeA #-}

-- | \(O(n)\). Traverse over elements and split the @Left@s and @Right@s.
mapEitherA
  :: Applicative f => (a -> f (Either b c)) -> Seq a -> f (Seq b, Seq c)
mapEitherA f = \case
  Tree x xs -> (\g -> Ap.liftA2 g (f x) (T.mapEitherA f xs)) $ \mx z ->
    case mx of
      Left x' -> unS2 $ bimap (Tree x') fromTree z
      Right x' -> unS2 $ bimap fromTree (Tree x') z
  Empty -> pure (Empty, Empty)
  where
    unS2 (U.S2 x y) = (x, y)
{-# INLINE mapEitherA #-}

-- | \(O(i + \log n)\). The longest prefix of elements that satisfy a predicate.
-- \(i\) is the length of the prefix.
--
-- ==== __Examples__
--
-- >>> takeWhile even (fromList [2,4,6,1,3,2,4])
-- [2,4,6]
takeWhile :: (a -> Bool) -> Seq a -> Seq a
takeWhile p t = IFo.ifoldr (\i x z -> if p x then z else take i t) t t
{-# INLINE takeWhile #-}

-- | \(O(i + \log n)\). The remainder after removing the longest prefix of
-- elements that satisfy a predicate.
-- \(i\) is the length of the prefix.
--
-- ==== __Examples__
--
-- >>> dropWhile even (fromList [2,4,6,1,3,2,4])
-- [1,3,2,4]
dropWhile :: (a -> Bool) -> Seq a -> Seq a
dropWhile p t = IFo.ifoldr (\i x z -> if p x then z else drop i t) Empty t
{-# INLINE dropWhile #-}

-- | \(O(i + \log n)\). The longest prefix of elements that satisfy a predicate,
-- together with the remainder of the sequence.
-- \(i\) is the length of the prefix.
--
-- @span p xs == ('takeWhile' p xs, 'dropWhile' p xs)@
--
-- ==== __Examples__
--
-- >>> span even (fromList [2,4,6,1,3,2,4])
-- ([2,4,6],[1,3,2,4])
span :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
span p t = IFo.ifoldr (\i x z -> if p x then z else splitAt i t) (t, Empty) t
{-# INLINE span #-}

-- | \(O(i + \log n)\). The longest prefix of elements that /do not/ satisfy a
-- predicate, together with the remainder of the sequence. \(i\) is the length
-- of the prefix.
--
-- @break p == 'span' (not . p)@
--
-- ==== __Examples__
--
-- >>> break odd (fromList [2,4,6,1,3,2,4])
-- ([2,4,6],[1,3,2,4])
break :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
break p = span (not . p)
{-# INLINE break #-}

-- | \(O(i + \log n)\). The longest suffix of elements that satisfy a predicate.
-- \(i\) is the length of the suffix.
takeWhileEnd :: (a -> Bool) -> Seq a -> Seq a
takeWhileEnd p t = IFo.ifoldl (\i z x -> if p x then z else drop (i+1) t) t t
{-# INLINE takeWhileEnd #-}

-- | \(O(i + \log n)\). The remainder after removing the longest suffix of
-- elements that satisfy a predicate.
-- \(i\) is the length of the suffix.
dropWhileEnd :: (a -> Bool) -> Seq a -> Seq a
dropWhileEnd p t =
  IFo.ifoldl (\i z x -> if p x then z else take (i+1) t) Empty t
{-# INLINE dropWhileEnd #-}

-- | \(O(i + \log n)\). The longest suffix of elements that satisfy a predicate,
-- together with the remainder of the sequence.
-- \(i\) is the length of the suffix.
--
-- @spanEnd p xs == ('dropWhileEnd' p xs, 'takeWhileEnd' p xs)@
spanEnd :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
spanEnd p t =
  IFo.ifoldl (\i z x -> if p x then z else splitAt (i+1) t) (Empty, t) t
{-# INLINE spanEnd #-}

-- | \(O(i + \log n)\). The longest suffix of elements that /do not/ satisfy a
-- predicate, together with the remainder of the sequence.
-- \(i\) is the length of the suffix.
--
-- @breakEnd p == 'spanEnd' (not . p)@
breakEnd :: (a -> Bool) -> Seq a -> (Seq a, Seq a)
breakEnd p = spanEnd (not . p)
{-# INLINE breakEnd #-}

--------------
-- Transform
--------------

-- | \(O(n)\). Reverse a sequence.
--
-- ==== __Examples__
--
-- >>> reverse (fromList [1,2,3,4,5])
-- [5,4,3,2,1]
reverse :: Seq a -> Seq a
reverse (Tree x xs) = case T.uncons (rev xs) of
  U.SNothing -> Tree x Tip
  U.SJust (U.S2 x' xs') -> Tree x' (T.snoc xs' x)
  where
    rev T.Tip = T.Tip
    rev (T.Bin sz y l r) = T.Bin sz y (rev r) (rev l)
reverse Empty = Empty

-- | \(O(n)\). Intersperse an element between the elements of a sequence.
--
-- ==== __Examples__
--
-- >>> intersperse '.' (fromList "HELLO")
-- "H.E.L.L.O"
intersperse :: a -> Seq a -> Seq a
intersperse y (Tree x xs) = case T.unsnoc (go xs) of
  U.SNothing -> error "Seq.intersperse: impossible"
  U.SJust (U.S2 xs' _) -> Tree x xs'
  where
    go T.Tip = T.Bin 1 y T.Tip T.Tip
    go (T.Bin sz z l r) = T.Bin (sz*2+1) z (go l) (go r)
    -- No need to balance, x <= 3y => 2x+1 <= 3(2y+1)
intersperse _ Empty = Empty

-- | \(O(n)\). Like 'Data.Foldable.foldl'' but keeps all intermediate values.
--
-- ==== __Examples__
--
-- >>> scanl (+) 0 (fromList [1..5])
-- [0,1,3,6,10,15]
scanl :: (b -> a -> b) -> b -> Seq a -> Seq b
scanl f !z0 =
  cons z0 .
  flip U.evalSState z0 .
  Tr.traverse (\x -> U.sState (\z -> let z' = f z x in U.S2 z' z'))
{-# INLINE scanl #-}

-- Note [SState for scans]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- SState is better than Trans.State.Strict.
-- For example, for scanl (+) (0 :: Int), the accumulator Int is unboxed with
-- SState but not with Trans.State.Strict.

-- | \(O(n)\). Like 'Data.Foldable.foldr'' but keeps all intermediate values.
--
-- ==== __Examples__
--
-- >>> scanr (+) 0 (fromList [1..5])
-- [15,14,12,9,5,0]
scanr :: (a -> b -> b) -> b -> Seq a -> Seq b
scanr f !z0 =
  flip snoc z0 .
  flip U.evalSState z0 .
  forwards .
  Tr.traverse
    (\x -> Backwards (U.sState (\z -> let z' = f x z in U.S2 z' z')))
{-# INLINE scanr #-}
-- See Note [SState for scans]

-- | \(O(n \log n)\). Sort a sequence. The sort is stable.
--
-- ==== __Examples__
--
-- >>> sort (fromList [4,2,3,5,1])
-- [1,2,3,4,5]
sort :: Ord a => Seq a -> Seq a
sort = sortBy compare
{-# INLINABLE sort #-}

-- | \(O(n \log n)\). Sort a sequence using a comparison function. The sort is
-- stable.
--
-- ==== __Examples__
--
-- >>> import Data.Ord (Down, comparing)
-- >>> sortBy (comparing Down) (fromList [4,2,3,5,1])
-- [5,4,3,2,1]
sortBy :: (a -> a -> Ordering) -> Seq a -> Seq a
sortBy cmp xs = IFu.imap (\i _ -> A.indexArray xa i) xs
  where
    n = length xs
    xa = A.createArray n errorElement $ \ma@(A.MutableArray ma#) -> do
      IFo.ifoldr (\i x z -> A.writeArray ma i x *> z) (pure ()) xs
      Sam.sortArrayBy cmp ma# 0 n
{-# INLINABLE sortBy #-}

-- Note [Inlinable sortBy]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- Don't INLINE sortBy because sortArrayBy is huge. The user can use Exts.inline
-- if they like.

--------------------
-- Search and test
--------------------

-- | \(O(n)\). The last element satisfying a predicate.
--
-- To get the first element, use @Data.Foldable.'Data.Foldable.find'@.
findEnd :: (a -> Bool) -> Seq a -> Maybe a
findEnd f =
  Monoid.getLast . foldMap (\x -> Monoid.Last (if f x then Just x else Nothing))
{-# INLINE findEnd #-}

-- | \(O(n)\). The index of the first element satisfying a predicate.
--
-- ==== __Examples__
--
-- >>> findIndex even (fromList [1..5])
-- Just 1
-- >>> findIndex (<0) (fromList [1..5])
-- Nothing
findIndex :: (a -> Bool) -> Seq a -> Maybe Int
findIndex f =
  Monoid.getFirst .
  IFo.ifoldMap (\i x -> Monoid.First (if f x then Just i else Nothing))
{-# INLINE findIndex #-}

-- | \(O(n)\). The index of the last element satisfying a predicate.
findIndexEnd :: (a -> Bool) -> Seq a -> Maybe Int
findIndexEnd f =
  Monoid.getLast .
  IFo.ifoldMap (\i x -> Monoid.Last (if f x then Just i else Nothing))
{-# INLINE findIndexEnd #-}

-- | \(O(n_1 + n_2)\). Indices in the second sequence where the first sequence
-- begins as a substring. Includes overlapping occurences.
--
-- ==== __Examples__
--
-- >>> infixIndices (fromList "ana") (fromList "banana")
-- [1,3]
-- >>> infixIndices (fromList [0]) (fromList [1,2,3])
-- []
-- >>> infixIndices (fromList "") (fromList "abc")
-- [0,1,2,3]
infixIndices :: Eq a => Seq a -> Seq a -> [Int]
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

infixIndicesMkArray :: Int -> Seq a -> A.Array a
infixIndicesMkArray !n !t = A.createArray n errorElement $ \ma ->
  IFo.ifoldr (\i x z -> A.writeArray ma i x *> z) (pure ()) t

-- | \(O(\log n)\). Binary search for an element in a sequence.
--
-- Given a function @f@ this function returns an arbitrary element @x@, if it
-- exists, such that @f x = EQ@. @f@ must be monotonic on the sequence—
-- specifically @fmap f@ must result in a sequence which has many (possibly
-- zero) @LT@s, followed by many @EQ@s, followed by many @GT@s.
--
-- ==== __Examples__
--
-- >>> binarySearchFind (`compare` 8) (fromList [2,4..10])
-- Just 8
-- >>> binarySearchFind (`compare` 3) (fromList [2,4..10])
-- Nothing
binarySearchFind :: (a -> Ordering) -> Seq a -> Maybe a
binarySearchFind f t = case t of
  Empty -> Nothing
  Tree x xs -> case f x of
    LT -> go xs
    EQ -> Just x
    GT -> Nothing
  where
    go Tip = Nothing
    go (Bin _ y l r) = case f y of
      LT -> go r
      EQ -> Just y
      GT -> go l
{-# INLINE binarySearchFind #-}

-- | \(O(\min(n_1,n_2))\). Whether the first sequence is a prefix of the second.
--
-- ==== __Examples__
--
-- >>> fromList "has" `isPrefixOf` fromList "haskell"
-- True
-- >>> fromList "ask" `isPrefixOf` fromList "haskell"
-- False
isPrefixOf :: Eq a => Seq a -> Seq a -> Bool
isPrefixOf t1 t2 =
  compareLength t1 t2 /= GT && Stream.isPrefixOf (stream t1) (stream t2)
{-# INLINABLE isPrefixOf #-}

-- | \(O(\min(n_1,n_2))\). Whether the first sequence is a suffix of the second.
--
-- ==== __Examples__
--
-- >>> fromList "ell" `isSuffixOf` fromList "haskell"
-- True
-- >>> fromList "ask" `isSuffixOf` fromList "haskell"
-- False
isSuffixOf :: Eq a => Seq a -> Seq a -> Bool
isSuffixOf t1 t2 =
  compareLength t1 t2 /= GT && Stream.isPrefixOf (streamEnd t1) (streamEnd t2)
{-# INLINABLE isSuffixOf #-}

-- | \(O(n_1 + n_2)\). Whether the first sequence is a substring of the second.
--
-- ==== __Examples__
--
-- >>> fromList "meow" `isInfixOf` fromList "homeowner"
-- True
-- >>> fromList [2,4] `isInfixOf` fromList [2,3,4]
-- False
isInfixOf :: Eq a => Seq a -> Seq a -> Bool
isInfixOf t1 t2 = not (null (infixIndices t1 t2))
{-# INLINABLE isInfixOf #-}

-- | \(O(n_1 + n_2)\). Whether the first sequence is a subsequence of the
-- second.
--
-- ==== __Examples__
--
-- >>> fromList [2,4] `isSubsequenceOf` [2,3,4]
-- True
-- >>> fromList "tab" `isSubsequenceOf` fromList "bat"
-- False
isSubsequenceOf :: Eq a => Seq a -> Seq a -> Bool
isSubsequenceOf t1 t2 =
  compareLength t1 t2 /= GT && Stream.isSubsequenceOf (stream t1) (stream t2)
{-# INLINABLE isSubsequenceOf #-}

--------
-- Zip
--------

-- | \(O(\min(n_1,n_2))\). Zip two sequences. The result is as long as the
-- shorter sequence.
zip :: Seq a -> Seq b -> Seq (a, b)
zip t1 t2 = zipWith (,) t1 t2

-- | \(O(\min(n_1,n_2,n_3))\). Zip three sequences. The result is as long as the
-- shortest sequence.
zip3 :: Seq a -> Seq b -> Seq c -> Seq (a, b, c)
zip3 t1 t2 t3 = zipWith3 (,,) t1 t2 t3

-- | \(O(\min(n_1,n_2))\). Zip two sequences with a function. The result is
-- as long as the shorter sequence.
--
-- ==== __Examples__
--
-- >>> zipWith (+) (fromList [1,2,3]) (fromList [1,1,1,1,1])
-- [2,3,4]
zipWith :: (a -> b -> c) -> Seq a -> Seq b -> Seq c
zipWith =
  (coerce :: ((a -> b -> Identity c) -> Seq a -> Seq b -> Identity (Seq c))
          -> (a -> b -> c) -> Seq a -> Seq b -> Seq c)
  zipWithM
{-# INLINE zipWith #-}

-- | \(O(\min(n_1,n_2,n_3))\). Zip three sequences with a function. The result
-- is as long as the shortest sequence.
zipWith3 :: (a -> b -> c -> d) -> Seq a -> Seq b -> Seq c -> Seq d
zipWith3 =
  (coerce :: ((a -> b -> c -> Identity d) -> Seq a -> Seq b -> Seq c -> Identity (Seq d))
          -> (a -> b -> c -> d) -> Seq a -> Seq b -> Seq c -> Seq d)
  zipWith3M
{-# INLINE zipWith3 #-}

-- | \(O(\min(n_1,n_2))\). Zip two sequences with a monadic function. The result
-- is as long as the shorter sequence.
zipWithM :: Monad m => (a -> b -> m c) -> Seq a -> Seq b -> m (Seq c)
zipWithM f t1 t2 = zipWithStreamM f t1 (stream t2)
{-# INLINE zipWithM #-}

-- | \(O(\min(n_1,n_2,n_3))\). Zip three sequences with a monadic function. The
-- result is as long as the shortest sequence.
zipWith3M
  :: Monad m => (a -> b -> c -> m d) -> Seq a -> Seq b -> Seq c -> m (Seq d)
zipWith3M f t1 t2 t3 =
  zipWithStreamM
    (\x (U.S2 y z) -> f x y z)
    t1
    (Stream.zipWith U.S2 (stream t2) (stream t3))
{-# INLINE zipWith3M #-}

zipWithStreamM :: Monad m => (a -> b -> m c) -> Seq a -> Stream b -> m (Seq c)
zipWithStreamM f t strm = case t of
  Empty -> pure Empty
  Tree x xs -> case strm of
    Stream step s -> case step s of
      Done -> pure Empty
      Yield y s1 ->
        Ap.liftA2 Tree (f x y) (T.zipWithStreamM f xs (Stream step s1))
{-# INLINE zipWithStreamM #-}

-- | \(O(n)\). Unzip a sequence of pairs.
unzip :: Seq (a, b) -> (Seq a, Seq b)
unzip t = unzipWith id t

-- | \(O(n)\). Unzip a sequence of triples.
unzip3 :: Seq (a, b, c) -> (Seq a, Seq b, Seq c)
unzip3 t = unzipWith3 id t

-- | \(O(n)\). Map over a sequence and unzip the result.
--
-- ==== __Examples__
--
-- >>> unzipWith (\x -> (x-1, x*2)) (fromList [1..5])
-- ([0,1,2,3,4],[2,4,6,8,10])
unzipWith :: (a -> (b, c)) -> Seq a -> (Seq b, Seq c)
unzipWith f t = case t of
  Tree x xs ->
    case (f x, T.unzipWithA (Identity U.#. f) xs) of
      ((x1,x2), Identity (U.S2 xs1 xs2)) ->
        let !t1 = Tree x1 xs1
            !t2 = Tree x2 xs2
        in (t1,t2)
  Empty -> (Empty, Empty)
{-# INLINE unzipWith #-}

-- | \(O(n)\). Map over a sequence and unzip the result.
unzipWith3 :: (a -> (b, c, d)) -> Seq a -> (Seq b, Seq c, Seq d)
unzipWith3 f t = case t of
  Tree x xs ->
    case (f x, T.unzipWith3A (Identity U.#. f) xs) of
      ((x1,x2,x3), Identity (U.S3 xs1 xs2 xs3)) ->
        let !t1 = Tree x1 xs1
            !t2 = Tree x2 xs2
            !t3 = Tree x3 xs3
        in (t1,t2,t3)
  Empty -> (Empty, Empty, Empty)
{-# INLINE unzipWith3 #-}

--------
-- Util
--------

fromTree :: Tree a -> Seq a
fromTree t = case T.uncons t of
  U.SNothing -> Empty
  U.SJust (U.S2 x xs) -> Tree x xs
{-# INLINE fromTree #-}

-- Note [compareLength]
-- ~~~~~~~~~~~~~~~~~~~~
-- The following functions exist for a bit of efficiency. GHC generates some
-- unnecessary branches for the simple `compare (length x) (length y)`, because
-- it does not know that the size of a Bin is always > the size of a Tip.

compareLength :: Seq a -> Seq b -> Ordering
compareLength l r = case l of
  Tree _ xs -> case r of
    Tree _ ys -> compareSize xs ys
    Empty -> GT
  Empty -> case r of
    Tree _ _ -> LT
    Empty -> EQ
{-# INLINE compareLength #-}

compareSize :: Tree a -> Tree b -> Ordering
compareSize l r = case l of
  Bin szl _ _ _ -> case r of
    Bin szr _ _ _ -> compare szl szr
    Tip -> GT
  Tip -> case r of
    Bin _ _ _ _ -> LT
    Tip -> EQ
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

-- Note [fromList implementation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- fromList is implemented by keeping a Stack where Seqs at each level have
-- their size as a power of 2. The powers of 2 increase down the stack. New
-- elements are pushed on the stack as Seqs of size 1. They are linked down the
-- stack when they match the next Seq's size. Every such link links two perfect
-- binary trees in O(1) using Bin. For n elements this takes O(n).
--
-- At the end, the Seqs in the stack are linked together from small to large,
-- balancing as necessary. Linking two Seqs A and B takes
-- O(|log(size A) - log(size B)|). The sizes of the Seqs in the stack are the
-- component powers of 2 of the total size n.
-- Let there be k powers of 2 in n, i.e. n = \sum_{i=1}^k p_i.
-- Then the total cost of the links is
--   O(\sum_{i=2}^k (\log 2^{p_i} - \log (\sum_{j=1}^{i-1} 2^{p_j})))
-- = O(\sum_{i=2}^k (\log 2^{p_i} - \log 2^{p_{i-1}}))
-- = O(\sum_{i=2}^k (p_i - p_{i-1}))
-- = O(p_k - p_1)
-- = O(\log n)

-- Note [concatMap implementation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The concatMap implementation is not unlike the fromList implementation.
-- Since arbitrary sized Seqs have to be concatenated, the Stack will not have
-- trees of sizes as perfect powers of 2. Instead, an invariant is maintained
-- that 2*size(stack!!0) <= size(stack!!1). This keeps the depth of the stack
-- bounded by O(log N), where N is the total size of Seqs in the stack.
--
-- If a new Seq is to be added and it is small enough to not violate the
-- invariant, it is simply pushed on the stack. If it would violate the
-- invariant, Seqs on the stack are linked to restore it. The idea here is
-- to merge Seqs of similar sizes as much as possible, since we want to minimize
-- the cost of linking which is O(|log(size A) - log(size B)|). The exact
-- strategy used for this is "2-merge", which has been described and
-- analyzed by Sam Buss and Alexander Knop in "Strategies for Stable Merge
-- Sorting" (https://arxiv.org/abs/1801.04641) for use in mergesort.
-- A merge strategy for mergesort hasn't been blindly adopted here. I arrived
-- at this strategy in an attempt to adopt fromList's implementation before I
-- was aware of the above paper. The incentives to merge similar sizes are very
-- clear here, more so compared to mergesort.
--
-- Finding a good complexity bound for this algorithm is a little tricky.
-- Given that we merge m Seqs of sizes n_i with total size N, I estimate the
-- complexity to be
-- O(log N - log_{n_m} + \sum_{i=1}^m \max(1, log_{n_i} - log_{n_{i-1})).
-- The log_{n_i} - log_{n_{i-1}} term is an upper bound on the cost of restoring
-- invariants when adding Seqs that turn out to be too big.
-- The log N - log_{n_m} is the final linking cost.
-- A special case of this is if all n_i are equal, say s. The complexity becomes
-- O(log sm - log s + m) = O(m).
-- The worst case occurs when Seqs of size 1 are interleaved into a sequence of
-- Seqs. The complexity becomes O(\sum_{i=1}^m log_{n_i}).

ltrPush :: Stack a -> a -> Stack a
ltrPush stk y = case stk of
  Push x Tip stk' -> ltrPushLoop stk' x 1 (T.Bin 1 y Tip Tip)
  _ -> Push y Tip stk

ltrPushLoop :: Stack a -> a -> Int -> Tree a -> Stack a
ltrPushLoop stk y !ysz ys = case stk of
  Push x xs@(Bin xsz _ _ _) stk'
    | xsz == ysz -> ltrPushLoop stk' x sz (Bin sz y xs ys)
    where
      sz = xsz+xsz+1
  _ -> Push y ys stk

rtlPush :: a -> Stack a -> Stack a
rtlPush x = \case
  Push y Tip stk' -> rtlPushLoop x 1 (T.Bin 1 y Tip Tip) stk'
  stk -> Push x Tip stk

rtlPushLoop :: a -> Int -> Tree a -> Stack a -> Stack a
rtlPushLoop x !xsz xs = \case
  Push y ys@(Bin ysz _ _ _) stk'
    | xsz == ysz -> rtlPushLoop x sz (Bin sz y xs ys) stk'
    where
      sz = xsz+xsz+1
  stk -> Push x xs stk

ltrPushMany :: Stack a -> a -> Tree a -> Stack a
ltrPushMany stk y ys = case stk of
  Push x xs stk'
    | ysz > xsz `div` 2 -> ltrPushManyLoop stk' x xsz xs y ysz ys
    | otherwise -> Push y ys stk
    where
      xsz = 1 + T.size xs
      ysz = 1 + T.size ys
  Nil -> Push y ys Nil

ltrPushManyLoop
  :: Stack a -> a -> Int -> Tree a -> a -> Int -> Tree a -> Stack a
ltrPushManyLoop stk y !ysz ys z !zsz zs = case stk of
  Push x xs@(Bin xsz1 _ _ _) stk'
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

ltrFinish :: Stack a -> Seq a
ltrFinish = wrapUpStack
  Empty
  U.S2
  (\(U.S2 y ys) x xs -> U.S2 x (T.link y xs ys))
  (\(U.S2 y ys) -> Tree y ys)

rtlFinish :: Stack a -> Seq a
rtlFinish = wrapUpStack
  Empty
  U.S2
  (\(U.S2 x xs) y ys -> U.S2 x (T.link y xs ys))
  (\(U.S2 x xs) -> Tree x xs)

-----------
-- Stream
-----------

-- Note [Streams]
-- ~~~~~~~~~~~~~~~~
-- Streams are used here for two reasons.
--
-- 1. It is better to implement lazy folds (foldr, foldl, etc) using Streams
--    rather than tree traversals. This is because they form loops which GHC
--    can optimize better on fusing with a consumer. For instance, the "cps
--    sum foldr" benchmark takes ~85% more time if foldr is implemented as a
--    recursive tree traversal. However, such an implementation is a little
--    faster for non-fusion use cases. For instance, the "foldr short-circuit"
--    benchmark takes ~30% less time. This behavior can be obtained when
--    desirable using foldMap with Endo.
-- 2. Streams can fuse for zip-like operations, so we use it to implement such
--    functions. These are decently fast, and we are saved from having to write
--    messy multi-tree traversals. Note that fold/build cannot fuse zips.
--    `zip = fromList (List.zip (toList t) (toList t))`, for instance, takes
--    ~40% more time compared to the stream-based zip.

stream :: Seq a -> Stream a
stream !t = Stream step s
  where
    s = case t of
      Tree x xs -> Push x xs Nil
      Empty -> Nil
    step = \case
      Push x xs stk -> let !stk' = down xs stk in Yield x stk'
      Nil -> Done
    {-# INLINE [0] step #-}
{-# INLINE stream #-}

streamEnd :: Seq a -> Stream a
streamEnd !t = Stream step s
  where
    s = case t of
      Tree x xs -> Push x xs Nil
      Empty -> Nil
    step = \case
      Push x xs stk -> case rDown x xs stk of
        U.S2 y stk' -> Yield y stk'
      Nil -> Done
    {-# INLINE [0] step #-}
{-# INLINE streamEnd #-}

down :: Tree a -> Stack a -> Stack a
down (Bin _ x l r) stk = down l (Push x r stk)
down Tip stk = stk

rDown :: a -> Tree a -> Stack a -> U.S2 a (Stack a)
rDown !y (Bin _ x l r) stk = rDown x r (Push y l stk)
rDown y Tip stk = U.S2 y stk

----------
-- Stack
----------

-- This is used in various places. What it stores depends on the specific use
-- case.
data Stack a = Push !a !(Tree a) !(Stack a) | Nil

wrapUpStack
  :: c -- empty
  -> (a -> Tree a -> b) -- initial
  -> (b -> a -> Tree a -> b) -- fold fun
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

valid :: Seq a -> Bool
valid = \case
  Tree _ xs -> T.valid xs
  Empty -> True

debugShowsPrec :: Show a => Int -> Seq a -> ShowS
debugShowsPrec p = \case
  Tree x xs ->
    showParen (p > 10) $
      showString "Tree " .
      showsPrec 11 x .
      showString " " .
      T.debugShowsPrec 11 xs
  Empty -> showString "Empty"

----------
-- Error
----------

errorElement :: a
errorElement = error "Seq: errorElement"
