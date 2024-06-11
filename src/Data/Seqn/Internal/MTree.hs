{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
module Data.Seqn.Internal.MTree
  (
    -- * Measured
    Measured(..)

    -- * MTree
  , MTree(..)

    -- * Basic
  , singleton
  , size
  , (<>>)
  , (<<>)
  , bin
  , binn

    -- * Folds
  , foldMap
  , foldl'
  , foldr'
  , ifoldl'
  , ifoldr'
  , traverse
  , ifoldMap
  , itraverse

    -- * Construct
  , generateA

    -- * Index
  , index
  , adjustF
  , insertAt
  , deleteAt

    -- * Slice
  , cons
  , snoc
  , uncons
  , unconsSure
  , unsnoc
  , unsnocSure
  , splitAtF

    -- * Transform
  , mapMaybeA
  , mapEitherA

    -- * Force
  , liftRnf2

    -- * Zip and unzip
  , zipWithStreamM
  , unzipWithA
  , unzipWith3A

    -- * Tree helpers
  , fold
  , foldSimple
  , link
  , merge
  , glue
  , balanceL
  , balanceR

    -- * Testing
  , valid
  , debugShowsPrec
  ) where

import Prelude hiding (foldMap, foldl', traverse)
import qualified Control.Applicative as Ap
import Control.DeepSeq (NFData(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Coerce (coerce)

import Data.Seqn.Internal.Stream (Stream(..), Step(..))
import qualified Data.Seqn.Internal.Util as U

-------------
-- Measured
-------------

-- | Types that have a combinable property, called the measure.
class Semigroup (Measure a) => Measured a where
  type Measure a

  -- | Calculate the measure of a value.
  measure :: a -> Measure a

----------
-- MTree
----------

data MTree a
  = MBin {-# UNPACK #-} !Int !(Measure a) !a !(MTree a) !(MTree a)
  | MTip

--------------
-- Instances
--------------

instance (NFData (Measure a), NFData a) => NFData (MTree a) where
  rnf = \case
    MBin _ v x l r -> rnf v `seq` rnf x `seq` rnf l `seq` rnf r
    MTip -> ()
  {-# INLINABLE rnf #-}

liftRnf2 :: (Measure a -> ()) -> (a -> ()) -> MTree a -> ()
liftRnf2 g f = go
  where
    go (MBin _ v x l r) = g v `seq` f x `seq` go l `seq` go r
    go MTip = ()
{-# INLINE liftRnf2 #-}

--------------
-- Basic ops
--------------

singleton :: Measured a => a -> MTree a
singleton x = MBin 1 (measure x) x MTip MTip
{-# INLINE singleton #-}

size :: MTree a -> Int
size (MBin n _ _ _ _) = n
size MTip = 0
{-# INLINE size #-}

infixr 6 <>>
infixr 6 <<>

(<>>) :: Measured a => MTree a -> Measure a -> Measure a
MBin _ v _ _ _ <>> x = v <> x
MTip           <>> x = x
{-# INLINE (<>>) #-}

(<<>) :: Measured a => Measure a -> MTree a -> Measure a
x <<> MBin _ v _ _ _ = x <> v
x <<> MTip           = x
{-# INLINE (<<>) #-}

-- O(1). Link two trees with a value in between. Precondition: The trees are
-- balanced wrt each other.
bin :: Measured a => a -> MTree a -> MTree a -> MTree a
bin x l r = MBin (size l + size r + 1) (l <>> measure x <<> r) x l r
{-# INLINE bin #-}

-- O(1). Link two trees with a value in between and a known total size.
-- Precondition: The trees are balanced wrt each other.
binn :: Measured a => Int -> a -> MTree a -> MTree a -> MTree a
binn n x l r = MBin n (l <>> measure x <<> r) x l r
{-# INLINE binn #-}

----------
-- Folds
----------

-- See Note [Folds] in Data.Seqn.Internal.Seq

foldl' :: (b -> a -> b) -> b -> MTree a -> b
foldl' f !z0 = \case
  MBin _ _ x l r -> go z0 x l r
  MTip -> z0
  where
    go !z !x l r = case l of
      MBin _ _ lx ll lr -> case r of
        MBin _ _ rx rl rr ->
          let !z' = go z lx ll lr
          in go (f z' x) rx rl rr
        MTip ->
          let !z' = go z lx ll lr
          in f z' x
      MTip -> case r of
        MBin _ _ rx rl rr -> go (f z x) rx rl rr
        MTip -> f z x
{-# INLINE foldl' #-}

ifoldl' :: (Int -> b -> a -> b) -> b -> Int -> MTree a -> b
ifoldl' f !z0 !i0 = \case
  MBin _ _ x l r -> go z0 i0 x l r
  MTip -> z0
  where
    go !z !i !x l r = case l of
      MBin lsz _ lx ll lr -> case r of
        MBin _ _ rx rl rr ->
          let !z' = go z i lx ll lr
          in go (f (i+lsz) z' x) (i+lsz+1) rx rl rr
        MTip ->
          let !z' = go z i lx ll lr
          in f (i+lsz) z' x
      MTip -> case r of
        MBin _ _ rx rl rr -> go (f i z x) (i+1) rx rl rr
        MTip -> f i z x
{-# INLINE ifoldl' #-}

foldr' :: (a -> b -> b) -> b -> MTree a -> b
foldr' f !z0 = \case
  MBin _ _ x l r -> go z0 x l r
  MTip -> z0
  where
    go !z !x l r = case l of
      MBin _ _ lx ll lr -> case r of
        MBin _ _ rx rl rr ->
          let !z' = go z rx rl rr
          in go (f x z') lx ll lr
        MTip -> go (f x z) lx ll lr
      MTip -> case r of
        MBin _ _ rx rl rr -> f x $! go z rx rl rr
        MTip -> f x z
{-# INLINE foldr' #-}

ifoldr' :: (Int -> a -> b -> b) -> b -> Int -> MTree a -> b
ifoldr' f !z0 !i0 = \case
  MBin _ _ x l r -> go z0 i0 x l r
  MTip -> z0
  where
    go !z !i !x l r = case l of
      MBin _ _ lx ll lr -> case r of
        MBin rsz _ rx rl rr ->
          let !z' = go z i rx rl rr
          in go (f (i-rsz) x z') (i-rsz-1) lx ll lr
        MTip -> go (f i x z) (i-1) lx ll lr
      MTip -> case r of
        MBin rsz _ rx rl rr -> f (i-rsz) x $! go z i rx rl rr
        MTip -> f i x z
{-# INLINE ifoldr' #-}

fold
  :: b
  -> (Int -> a -> b -> b -> b)
  -> (Int -> a -> b -> b)
  -> (Int -> a -> b -> b)
  -> (a -> b)
  -> MTree a
  -> b
fold tip glr gl gr g = \case
  MBin sz _ x l r -> go sz x l r
  MTip -> tip
  where
    go !sz !x l r = case l of
      MBin lsz _ lx ll lr -> case r of
        MBin rsz _ rx rl rr -> glr sz x (go lsz lx ll lr) (go rsz rx rl rr)
        MTip -> gl sz x (go lsz lx ll lr)
      MTip -> case r of
        MBin rsz _ rx rl rr -> gr sz x (go rsz rx rl rr)
        MTip -> g x
{-# INLINE fold #-}

foldSimple :: b -> (Int -> a -> b -> b -> b) -> MTree a -> b
foldSimple tip f = fold tip f gl gr g
  where
    gl !sz x ml = f sz x ml tip
    {-# INLINE gl #-}
    gr !sz x mr = f sz x tip mr
    {-# INLINE gr #-}
    g x = f 1 x tip tip
    {-# INLINE g #-}
{-# INLINE foldSimple #-}

foldMap :: forall a m. Monoid m => (a -> m) -> MTree a -> m
foldMap f = coerce (fold @m @a) (mempty @m) glr gl gr f
  where
    glr (_ :: Int) x l r = l <> f x <> r
    {-# INLINE glr #-}
    gl (_ :: Int) x l = l <> f x
    {-# INLINE gl #-}
    gr (_ :: Int) x r = f x <> r
    {-# INLINE gr #-}
{-# INLINE foldMap #-}

traverse :: (Measured b, Applicative f) => (a -> f b) -> MTree a -> f (MTree b)
traverse f = fold (pure MTip) glr gl gr g
  where
    glr !sz x ml mr = Ap.liftA3 (flip (binn sz)) ml (f x) mr
    {-# INLINE glr #-}
    gl !sz x ml = Ap.liftA2 (\l' x' -> binn sz x' l' MTip) ml (f x)
    {-# INLINE gl #-}
    gr !sz x mr = Ap.liftA2 (\x' r' -> binn sz x' MTip r') (f x) mr
    {-# INLINE gr #-}
    g x = fmap singleton (f x)
    {-# INLINE g #-}
{-# INLINE traverse #-}

ifoldMap :: Monoid m => (Int -> a -> m) -> Int -> MTree a -> m
ifoldMap f !i0 = \case
  MBin _ _ x l r -> go i0 x l r
  MTip -> mempty
  where
    go !i x l r = case l of
      MBin lsz _ lx ll lr -> case r of
        MBin _ _ rx rl rr ->
          go i lx ll lr <> f (i+lsz) x <> go (i+lsz+1) rx rl rr
        MTip -> go i lx ll lr <> f (i+lsz) x
      MTip -> case r of
        MBin _ _ rx rl rr -> f i x <> go (i+1) rx rl rr
        MTip -> f i x
{-# INLINE ifoldMap #-}

itraverse
  :: (Measured b, Applicative f)
  => (Int -> a -> f b) -> Int -> MTree a -> f (MTree b)
itraverse f !i0 = \case
  MBin sz _ x l r -> go i0 sz x l r
  MTip -> pure MTip
  where
    go !i !sz x l r = case l of
      MBin lsz _ lx ll lr -> case r of
        MBin rsz _ rx rl rr ->
          Ap.liftA3
            (flip (binn sz))
            (go i lsz lx ll lr)
            (f (i+lsz) x)
            (go (i+lsz+1) rsz rx rl rr)
        MTip ->
          Ap.liftA2
            (\l' x' -> binn sz x' l' MTip)
            (go i lsz lx ll lr)
            (f (i+lsz) x)
      MTip -> case r of
        MBin rsz _ rx rl rr ->
          Ap.liftA2
            (\x' r' -> binn sz x' MTip r')
            (f i x)
            (go (i+1) rsz rx rl rr)
        MTip ->
          fmap singleton (f i x)
{-# INLINE itraverse #-}

-----------------
-- Construction
-----------------

generateA
  :: (Measured a, Applicative f)
  => (Int -> f a) -> Int -> Int -> f (MTree a)
generateA f = go
  where
    go !i n
      | n <= 0 = pure MTip
      | otherwise =
          Ap.liftA3
            (flip (binn n))
            (go i lsz)
            (f (i+lsz))
            (go (i+lsz+1) (n-lsz-1))
      where
        lsz = (n-1) `div` 2
{-# INLINE generateA #-}

-------------
-- Indexing
-------------

-- Precondition: 0 <= i < size xs
index :: Int -> MTree a -> a
index !i = \case
  MBin _ _ x l r -> case compare i szl of
    LT -> index i l
    EQ -> x
    GT -> index (i-szl-1) r
    where
      szl = size l
  MTip -> errorOutOfBounds "MTree.index"

-- Precondition: 0 <= i < size xs
adjustF
  :: (Measured a, Functor f)
  => (a -> f a) -> Int -> MTree a -> f (MTree a)
adjustF f = go
  where
    go !i = \case
      MBin sz _ x l r -> case compare i szl of
        LT -> fmap (\l' -> binn sz x l' r) (go i l)
        EQ -> fmap (\x' -> binn sz x' l r) (f x)
        GT -> fmap (binn sz x l) (go (i-szl-1) r)
        where
          szl = size l
      MTip -> errorOutOfBounds "MTree.adjustF"
{-# INLINE adjustF #-}

-- Inserts at ends if not in bounds
insertAt :: Measured a => Int -> a -> MTree a -> MTree a
insertAt !i x (MBin _ _ y l r)
  | i <= szl = balanceL y (insertAt i x l) r
  | otherwise = balanceR y l (insertAt (i-szl-1) x r)
  where
    szl = size l
insertAt _ x MTip = singleton x
{-# INLINABLE insertAt #-}

-- Precondition: 0 <= i < size xs
deleteAt :: Measured a => Int -> MTree a -> MTree a
deleteAt !i (MBin _ _ x l r) = case compare i szl of
  LT -> balanceR x (deleteAt i l) r
  EQ -> glue l r
  GT -> balanceL x l (deleteAt (i-szl-1) r)
  where
    szl = size l
deleteAt _ MTip = errorOutOfBounds "MTree.deleteAt"
{-# INLINABLE deleteAt #-}

----------
-- Slice
----------

cons :: Measured a => a -> MTree a -> MTree a
cons x MTip = singleton x
cons x (MBin _ _ y l r) = balanceL y (cons x l) r
{-# INLINABLE cons #-}

snoc :: Measured a => MTree a -> a -> MTree a
snoc MTip x = singleton x
snoc (MBin _ _ y l r) x = balanceR y l (snoc r x)
{-# INLINABLE snoc #-}

uncons :: Measured a => MTree a -> U.SMaybe (U.S2 a (MTree a))
uncons (MBin _ _ x l r) = U.SJust (unconsSure x l r)
uncons MTip = U.SNothing
{-# INLINE uncons #-}

unconsSure :: Measured a => a -> MTree a -> MTree a -> U.S2 a (MTree a)
unconsSure x (MBin _ _ lx ll lr) r = case unconsSure lx ll lr of
  U.S2 y l' -> U.S2 y (balanceR x l' r)
unconsSure x MTip r = U.S2 x r
{-# INLINABLE unconsSure #-}

unsnoc :: Measured a => MTree a -> U.SMaybe (U.S2 (MTree a) a)
unsnoc (MBin _ _ x l r) = U.SJust $ unsnocSure x l r
unsnoc MTip = U.SNothing
{-# INLINE unsnoc #-}

unsnocSure :: Measured a => a -> MTree a -> MTree a -> U.S2 (MTree a) a
unsnocSure x l (MBin _ _ rx rl rr) = case unsnocSure rx rl rr of
  U.S2 r' y -> U.S2 (balanceL x l r') y
unsnocSure x l MTip = U.S2 l x
{-# INLINABLE unsnocSure #-}

-- Precondition: 0 <= i < size xs
splitAtF
  :: (Measured a, U.Biapplicative f)
  => Int -> MTree a -> f (MTree a) (U.S2 a (MTree a))
splitAtF = go
  where
    go !i (MBin _ _ x l r) = case compare i szl of
      LT -> second (second (\lr -> link x lr r)) (go i l)
      EQ -> U.bipure l (U.S2 x r)
      GT -> first (link x l) (go (i-szl-1) r)
      where
        szl = size l
    go _ MTip = errorOutOfBounds "MTree.splitAtF"
{-# INLINE splitAtF #-}

--------------
-- Transform
--------------

mapMaybeA
  :: (Applicative f, Measured b)
  => (a -> f (Maybe b)) -> MTree a -> f (MTree b)
mapMaybeA f = foldSimple tip g
  where
    tip = pure MTip
    {-# INLINE tip #-}
    g _ x ml mr = (\h -> Ap.liftA3 h ml (f x) mr) $ \l my r ->
      case my of
        Nothing -> merge l r
        Just y -> link y l r
    {-# INLINE g #-}
{-# INLINE mapMaybeA #-}

mapEitherA
  :: (Applicative f, Measured b, Measured c)
  => (a -> f (Either b c)) -> MTree a -> f (U.S2 (MTree b) (MTree c))
mapEitherA f = foldSimple tip g
  where
    tip = pure (U.bipure MTip MTip)
    {-# INLINE tip #-}
    g _ x ml mr = (\h -> Ap.liftA3 h ml (f x) mr) $ \l my r ->
      case my of
        Left y -> U.biliftA2 (link y) merge l r
        Right y -> U.biliftA2 merge (link y) l r
    {-# INLINE g #-}
{-# INLINE mapEitherA #-}

------------------
-- Zip and unzip
------------------

zipWithStreamM
  :: (Measured c, Monad m)
  => (a -> b -> m c) -> MTree a -> Stream b -> m (MTree c)
zipWithStreamM f t (Stream step s) = U.evalSStateT (foldSimple tip g t) s
  where
    tip = pure MTip
    {-# INLINE tip #-}
    g _ x ml mr = U.SStateT $ \s2 -> do
      U.S2 s3 l <- U.runSStateT ml s2
      case step s3 of
        Done -> pure $ U.S2 s3 l
        Yield y s4 -> do
          z <- f x y
          U.S2 s5 r <- U.runSStateT mr s4
          pure $! U.S2 s5 (link z l r)
    {-# INLINE g #-}
{-# INLINE zipWithStreamM #-}

unzipWithA
  :: (Measured b, Measured c, Applicative f)
  => (a -> f (b, c)) -> MTree a -> f (U.S2 (MTree b) (MTree c))
unzipWithA f = foldSimple tip g
  where
    tip = pure (U.S2 MTip MTip)
    {-# INLINE tip #-}
    g !sz x ml mr = Ap.liftA3 bin2 ml (f x) mr
      where
        bin2 (U.S2 l1 l2) (x1,x2) (U.S2 r1 r2) =
          U.S2 (binn sz x1 l1 r1) (binn sz x2 l2 r2)
    {-# INLINE g #-}
{-# INLINE unzipWithA #-}

unzipWith3A
  :: (Measured b, Measured c, Measured d, Applicative f)
  => (a -> f (b, c, d))
  -> MTree a
  -> f (U.S3 (MTree b) (MTree c) (MTree d))
unzipWith3A f = foldSimple tip g
  where
    tip = pure (U.S3 MTip MTip MTip)
    {-# INLINE tip #-}
    g !sz x ml mr = Ap.liftA3 bin3 ml (f x) mr
      where
        bin3 (U.S3 l1 l2 l3) (x1,x2,x3) (U.S3 r1 r2 r3) =
          U.S3 (binn sz x1 l1 r1) (binn sz x2 l2 r2) (binn sz x3 l3 r3)
    {-# INLINE g #-}
{-# INLINE unzipWith3A #-}

-----------
-- Errors
-----------

errorOutOfBounds :: String -> a
errorOutOfBounds name = error (name ++ ": out of bounds")

------------
-- Balance
------------

-- O(|log n1 - log n2|). Link two trees with a value in between.
link :: Measured a => a -> MTree a -> MTree a -> MTree a
link !x MTip r = cons x r
link x l MTip = snoc l x
link x l@(MBin ls lv lx ll lr) r@(MBin rs rv rx rl rr)
  | delta*ls < rs = balanceL rx (linkL x ls l rl) rr
  | delta*rs < ls = balanceR lx ll (linkR x lr rs r)
  | otherwise     = MBin (1+ls+rs) (lv <> measure x <> rv) x l r
{-# INLINE link #-}

linkL :: Measured a => a -> Int -> MTree a -> MTree a -> MTree a
linkL !x !ls !l r = case r of
  MBin rs rv rx rl rr
    | delta*ls < rs -> balanceL rx (linkL x ls l rl) rr
    | otherwise     -> MBin (1+ls+rs) (l <>> measure x <> rv) x l r
  MTip -> error "MTree.linkL: impossible"
{-# INLINABLE linkL #-}

linkR :: Measured a => a -> MTree a -> Int -> MTree a -> MTree a
linkR !x l !rs !r = case l of
  MBin ls lv lx ll lr
    | delta*rs < ls -> balanceR lx ll (linkR x lr rs r)
    | otherwise     -> MBin (1+ls+rs) (lv <> measure x <<> r) x l r
  MTip -> error "MTree.linkR: impossible"
{-# INLINABLE linkR #-}

-- O(log (n1 + n2)). Link two trees.
merge :: Measured a => MTree a -> MTree a -> MTree a
merge MTip r = r
merge l MTip = l
merge l@(MBin ls _ lx ll lr) r@(MBin rs _ rx rl rr)
  | ls < rs = case unsnocSure lx ll lr of U.S2 l' mx -> link mx l' r
  | otherwise = case unconsSure rx rl rr of U.S2 mx r' -> link mx l r'
{-# INLINE merge #-}

-- O(log (n1 + n2)). Link two trees. Precondition: The trees must be balanced
-- wrt each other.
glue :: Measured a => MTree a -> MTree a -> MTree a
glue MTip r = r
glue l MTip = l
glue l@(MBin ls _ lx ll lr) r@(MBin rs _ rx rl rr)
  | ls > rs = case unsnocSure lx ll lr of U.S2 l' m -> balanceR m l' r
  | otherwise = case unconsSure rx rl rr of U.S2 m r' -> balanceL m l r'
{-# INLINE glue #-}

-- See Note [Balance] in Data.Seqn.Internal.Tree
delta, ratio :: Int
delta = 3
ratio = 2

-- O(1). Restores balance with at most one right rotation. Precondition: One
-- right rotation must be enough to restore balance. This is the case when the
-- left tree might have been inserted to or the right tree deleted from.
balanceL :: Measured a => a -> MTree a -> MTree a -> MTree a
balanceL !x l r = case r of
  MTip -> case l of
    MTip -> MBin 1 v x MTip MTip
    MBin _ lv lx ll lr -> case lr of
      MTip -> case ll of
        MTip -> MBin 2 (lv <> v) x l MTip
        MBin _ _ _ _ _ ->
          MBin 3 (lv <> v) lx ll (MBin 1 v x MTip MTip)
      MBin _ lrv lrx _ _ -> case ll of
        MTip ->
          MBin 3
               (lv <> v)
               lrx
               (MBin 1 (measure lx) lx MTip MTip)
               (MBin 1 v x MTip MTip)
        MBin _ _ _ _ _ ->
          MBin 4 (lv <> v) lx ll (MBin 2 (lrv <> measure x) x lr MTip)
  MBin rs rv _ _ _ -> case l of
    MTip -> MBin (1+rs) (v <> rv) x MTip r
    MBin ls lv lx ll lr
      | ls > delta*rs -> case (ll, lr) of
        (MBin lls llv _ _ _, MBin lrs lrv lrx lrl lrr)
          | lrs < ratio*lls ->
            MBin (1+ls+rs)
                 (lv <> v <> rv)
                 lx
                 ll
                 (MBin (1+rs+lrs) (lrv <> v <> rv) x lr r)
          | otherwise ->
            MBin (1+ls+rs)
                 (lv <> v <> rv)
                 lrx
                 (MBin (1+lls+size lrl) (llv <> measure lx <<> lrl) lx ll lrl)
                 (MBin (1+rs+size lrr) (lrr <>> v <> rv) x lrr r)
        _ -> error "MTree.balanceL: impossible"
      | otherwise -> MBin (1+ls+rs) (lv <> v <> rv) x l r
  where
    v = measure x
{-# INLINABLE balanceL #-}

-- O(1). Restores balance with at most one left rotation. Precondition: One left
-- rotation must be enough to restore balance. This is the case when the right
-- tree might have been inserted to or the left tree deleted from.
balanceR :: Measured a => a -> MTree a -> MTree a -> MTree a
balanceR !x l r = case l of
  MTip -> case r of
    MTip -> MBin 1 v x MTip MTip
    MBin _ rv rx rl rr -> case rl of
      MTip -> case rr of
        MTip -> MBin 2 (v <> rv) x MTip r
        MBin _ _ _ _ _ -> MBin 3 (v <> rv) rx (MBin 1 v x MTip MTip) rr
      MBin _ rlv rlx _ _ -> case rr of
        MTip ->
          MBin 3
               (v <> rv)
               rlx
               (MBin 1 v x MTip MTip)
               (MBin 1 (measure rx) rx MTip MTip)
        MBin _ _ _ _ _ ->
          MBin 4 (v <> rv) rx (MBin 2 (v <> rlv) x MTip rl) rr
  MBin ls lv _ _ _ -> case r of
    MTip -> MBin (1+ls) (lv <> v) x l MTip
    MBin rs rv rx rl rr
      | rs > delta*ls -> case (rl, rr) of
        (MBin rls rlv rlx rll rlr, MBin rrs rrv _ _ _)
          | rls < ratio*rrs ->
            MBin (1+ls+rs)
                 (lv <> v <> rv)
                 rx
                 (MBin (1+ls+rls) (lv <> v <> rlv) x l rl)
                 rr
          | otherwise ->
            MBin (1+ls+rs)
                 (lv <> v <> rv)
                 rlx
                 (MBin (1+ls+size rll) (lv <> v <<> rll) x l rll)
                 (MBin (1+rrs+size rlr) (rlr <>> measure rx <> rrv) rx rlr rr)
        _ -> error "MTree.balanceR: impossible"
      | otherwise -> MBin (1+ls+rs) (lv <> v <> rv) x l r
  where
    v = measure x
{-# INLINABLE balanceR #-}

------------
-- Testing
------------

valid :: (Measured a, Eq (Measure a)) => MTree a -> Bool
valid s = balanceOk s && sizeOk s && measureOk s
  where
    balanceOk = \case
      MBin _ _ _ l r -> ok && balanceOk l && balanceOk r
        where
          ok = size l + size r <= 1 ||
               (size l <= delta * size r && size r <= delta * size l)
      MTip -> True

    sizeOk = \case
      MBin sz _ _ l r -> sizeOk l && sizeOk r && size l + size r + 1 == sz
      MTip -> True

    measureOk = \case
      MBin _ v x l r ->
        measureOk l && measureOk r && l <>> measure x <<> r == v
      MTip -> True

debugShowsPrec :: (Show a, Show (Measure a)) => Int -> MTree a -> ShowS
debugShowsPrec p = \case
  MBin sz v x l r ->
    showParen (p > 10) $
      showString "MBin " .
      shows sz .
      showString " " .
      showsPrec 11 v .
      showString " " .
      showsPrec 11 x .
      showString " " .
      debugShowsPrec 11 l .
      showString " " .
      debugShowsPrec 11 r
  MTip -> showString "MTip"
