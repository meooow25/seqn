{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
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
module Data.Seqn.Internal.Tree
  (
    -- * Tree
    Tree(..)

    -- * Basic
  , size
  , bin

    -- * Folds
  , foldl'
  , ifoldl'
  , foldr'
  , ifoldr'
  , traverse
  , itraverse

    -- * Construct
  , generateA

    -- * Index
  , adjustF
  , insertAt
  , deleteAt

    -- * Slice
  , cons
  , snoc
  , uncons
  , unsnoc
  , splitAtF

    -- * Transform
  , mapMaybeA
  , mapEitherA

    -- * Zip and unzip
  , zipWithStreamM
  , unzipWithA
  , unzipWith3A

    -- * Tree helpers
  , fold
  , foldSimple
  , link
  , glue
  , merge
  , balanceL
  , balanceR

    -- * Testing
  , valid
  , debugShowsPrec
  ) where

import Prelude hiding (concatMap, break, drop, dropWhile, filter, foldl', lookup, map, replicate, reverse, scanl, scanr, span, splitAt, take, takeWhile, traverse, unzip, unzip3, zip, zip3, zipWith, zipWith3)
import qualified Control.Applicative as Ap
import Control.DeepSeq (NFData(..), NFData1(..))
import Data.Bifunctor (Bifunctor(..))

import qualified Data.Seqn.Internal.Util as U
import Data.Seqn.Internal.Stream (Stream(..), Step(..))

data Tree a
  = Bin {-# UNPACK #-} !Int !a !(Tree a) !(Tree a)
  | Tip

--------------
-- Instances
--------------

instance NFData a => NFData (Tree a) where
  rnf = \case
    Bin _ x l r -> rnf x `seq` rnf l `seq` rnf r
    Tip -> ()
  {-# INLINABLE rnf #-}

instance NFData1 Tree where
  liftRnf f = go
    where
      go (Bin _ x l r) = f x `seq` go l `seq` go r
      go Tip = ()
  {-# INLINE liftRnf #-}

-------------------
-- Basic Tree ops
-------------------

singleton :: a -> Tree a
singleton x = Bin 1 x Tip Tip
{-# INLINE singleton #-}

size :: Tree a -> Int
size (Bin n _ _ _) = n
size Tip = 0
{-# INLINE size #-}

-- O(1). Link two trees with a value in between. Precondition: The trees are
-- balanced wrt each other.
bin :: a -> Tree a -> Tree a -> Tree a
bin x l r = Bin (size l + size r + 1) x l r
{-# INLINE bin #-}

----------
-- Folds
----------

-- Note [Folds]
-- ~~~~~~~~~~~~
-- Certain functions, such as folds, are implemented recursively on non-empty
-- trees, i.e. `go :: Int -> a -> Tree a -> Tree a -> b` instead of
-- `go :: Tree a -> b`. This is simply because benchmarks show this to be
-- faster.

foldl' :: (b -> a -> b) -> b -> Tree a -> b
foldl' f !z0 = \case
  Bin _ x l r -> go z0 x l r
  Tip -> z0
  where
    go !z !x l r = case l of
      Bin _ lx ll lr -> case r of
        Bin _ rx rl rr ->
          let !z' = go z lx ll lr
          in go (f z' x) rx rl rr
        Tip ->
          let !z' = go z lx ll lr
          in f z' x
      Tip -> case r of
        Bin _ rx rl rr -> go (f z x) rx rl rr
        Tip -> f z x
{-# INLINE foldl' #-}

ifoldl' :: (Int -> b -> a -> b) -> b -> Int -> Tree a -> b
ifoldl' f !z0 !i0 = \case
  Bin _ x l r -> go z0 i0 x l r
  Tip -> z0
  where
    go !z !i !x l r = case l of
      Bin lsz lx ll lr -> case r of
        Bin _ rx rl rr ->
          let !z' = go z i lx ll lr
          in go (f (i+lsz) z' x) (i+lsz+1) rx rl rr
        Tip ->
          let !z' = go z i lx ll lr
          in f (i+lsz) z' x
      Tip -> case r of
        Bin _ rx rl rr -> go (f i z x) (i+1) rx rl rr
        Tip -> f i z x
{-# INLINE ifoldl' #-}

foldr' :: (a -> b -> b) -> b -> Tree a -> b
foldr' f !z0 = \case
  Bin _ x l r -> go z0 x l r
  Tip -> z0
  where
    go !z !x l r = case l of
      Bin _ lx ll lr -> case r of
        Bin _ rx rl rr ->
          let !z' = go z rx rl rr
          in go (f x z') lx ll lr
        Tip -> go (f x z) lx ll lr
      Tip -> case r of
        Bin _ rx rl rr -> f x $! go z rx rl rr
        Tip -> f x z
{-# INLINE foldr' #-}

ifoldr' :: (Int -> a -> b -> b) -> b -> Int -> Tree a -> b
ifoldr' f !z0 !i0 = \case
  Bin _ x l r -> go z0 i0 x l r
  Tip -> z0
  where
    go !z !i !x l r = case l of
      Bin _ lx ll lr -> case r of
        Bin rsz rx rl rr ->
          let !z' = go z i rx rl rr
          in go (f (i-rsz) x z') (i-rsz-1) lx ll lr
        Tip -> go (f i x z) (i-1) lx ll lr
      Tip -> case r of
        Bin rsz rx rl rr -> f (i-rsz) x $! go z i rx rl rr
        Tip -> f i x z
{-# INLINE ifoldr' #-}

fold
  :: b
  -> (Int -> a -> b -> b -> b)
  -> (Int -> a -> b -> b)
  -> (Int -> a -> b -> b)
  -> (a -> b)
  -> Tree a
  -> b
fold tip glr gl gr g = \case
  Bin sz x l r -> go sz x l r
  Tip -> tip
  where
    go !sz !x l r = case l of
      Bin lsz lx ll lr -> case r of
        Bin rsz rx rl rr -> glr sz x (go lsz lx ll lr) (go rsz rx rl rr)
        Tip -> gl sz x (go lsz lx ll lr)
      Tip -> case r of
        Bin rsz rx rl rr -> gr sz x (go rsz rx rl rr)
        Tip -> g x
{-# INLINE fold #-}

foldSimple :: b -> (Int -> a -> b -> b -> b) -> Tree a -> b
foldSimple tip f = fold tip f gl gr g
  where
    gl !sz x ml = f sz x ml tip
    {-# INLINE gl #-}
    gr !sz x mr = f sz x tip mr
    {-# INLINE gr #-}
    g x = f 1 x tip tip
    {-# INLINE g #-}
{-# INLINE foldSimple #-}

traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
traverse f = fold (pure Tip) glr gl gr g
  where
    glr !sz x ml mr = liftA3R' (flip (Bin sz)) ml (f x) mr
    -- See Note [Traverse liftA3R']
    {-# INLINE glr #-}
    gl !sz x ml = Ap.liftA2 (\l' x' -> Bin sz x' l' Tip) ml (f x)
    {-# INLINE gl #-}
    gr !sz x mr = Ap.liftA2 (\x' r' -> Bin sz x' Tip r') (f x) mr
    {-# INLINE gr #-}
    g x = fmap singleton (f x)
    {-# INLINE g #-}
{-# INLINE traverse #-}

itraverse :: Applicative f => (Int -> a -> f b) -> Int -> Tree a -> f (Tree b)
itraverse f !i0 = \case
  Bin sz x l r -> go i0 sz x l r
  Tip -> pure Tip
  where
    go !i !sz x l r = case l of
      Bin lsz lx ll lr -> case r of
        Bin rsz rx rl rr ->
          liftA3R'
            (flip (Bin sz))
            (go i lsz lx ll lr)
            (f (i+lsz) x)
            (go (i+lsz+1) rsz rx rl rr)
          -- See Note [Traverse liftA3R']
        Tip ->
          Ap.liftA2
            (\l' x' -> Bin sz x' l' Tip)
            (go i lsz lx ll lr)
            (f (i+lsz) x)
      Tip -> case r of
        Bin rsz rx rl rr ->
          Ap.liftA2 (\x' r' -> Bin sz x' Tip r') (f i x) (go (i+1) rsz rx rl rr)
        Tip ->
          fmap (\x' -> Bin sz x' Tip Tip) (f i x)
    -- See Note [Traverse]
{-# INLINE itraverse #-}

-- Note [Traverse liftA3R']
-- ~~~~~~~~~~~~~~~~~~~~~~~~
-- We want to associate to the right because we define foldMap using traverse
-- and ifoldMap using itraverse. It is more appropriate to be right-associative
-- for <>.

-- Right associative and strict
liftA3R' :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3R' f mx my mz =
  Ap.liftA2
    (\x (U.S2 y z) -> f x y z)
    mx
    (Ap.liftA2 U.S2 my mz)
{-# INLINE liftA3R' #-}

--------------
-- Construct
--------------

generateA :: Applicative f => (Int -> f a) -> Int -> Int -> f (Tree a)
generateA f = go
  where
    go !i n
      | n <= 0 = pure Tip
      | otherwise =
          Ap.liftA3
            (flip (Bin n))
            (go i lsz)
            (f (i+lsz))
            (go (i+lsz+1) (n-lsz-1))
      where
        lsz = (n-1) `div` 2
{-# INLINE generateA #-}

----------
-- Index
----------

-- Precondition: 0 <= i < size xs
adjustF :: Functor f => (a -> f a) -> Int -> Tree a -> f (Tree a)
adjustF f = go
  where
    go !i = \case
      Bin sz x l r -> case compare i szl of
        LT -> fmap (\l' -> Bin sz x l' r) (go i l)
        EQ -> fmap (\x' -> Bin sz x' l r) (f x)
        GT -> fmap (Bin sz x l) (go (i-szl-1) r)
        where
          szl = size l
      Tip -> errorOutOfBounds "Tree.adjustF"
{-# INLINE adjustF #-}

-- Inserts at ends if not in bounds
insertAt :: Int -> a -> Tree a -> Tree a
insertAt !i x (Bin _ y l r)
  | i <= szl = balanceL y (insertAt i x l) r
  | otherwise = balanceR y l (insertAt (i-szl-1) x r)
  where
    szl = size l
insertAt _ x Tip = singleton x

-- Precondition: 0 <= i < size xs
deleteAt :: Int -> Tree a -> Tree a
deleteAt !i (Bin _ x l r) = case compare i szl of
  LT -> balanceR x (deleteAt i l) r
  EQ -> glue l r
  GT -> balanceL x l (deleteAt (i-szl-1) r)
  where
    szl = size l
deleteAt _ Tip = errorOutOfBounds "Tree.deleteAt"

----------
-- Slice
----------

cons :: a -> Tree a -> Tree a
cons x Tip = singleton x
cons x (Bin _ y l r) = balanceL y (cons x l) r

snoc :: Tree a -> a -> Tree a
snoc Tip x = singleton x
snoc (Bin _ y l r) x = balanceR y l (snoc r x)

uncons :: Tree a -> U.SMaybe (U.S2 a (Tree a))
uncons (Bin _ x l r) = U.SJust (unconsSure x l r)
uncons Tip = U.SNothing
{-# INLINE uncons #-}

unconsSure :: a -> Tree a -> Tree a -> U.S2 a (Tree a)
unconsSure x (Bin _ lx ll lr) r = case unconsSure lx ll lr of
  U.S2 y l' -> U.S2 y (balanceR x l' r)
unconsSure x Tip r = U.S2 x r

unsnoc :: Tree a -> U.SMaybe (U.S2 (Tree a) a)
unsnoc (Bin _ x l r) = U.SJust $ unsnocSure x l r
unsnoc Tip = U.SNothing
{-# INLINE unsnoc #-}

unsnocSure :: a -> Tree a -> Tree a -> U.S2 (Tree a) a
unsnocSure x l (Bin _ rx rl rr) = case unsnocSure rx rl rr of
  U.S2 r' y -> U.S2 (balanceL x l r') y
unsnocSure x l Tip = U.S2 l x

-- Precondition: 0 <= i < size xs
splitAtF
  :: U.Biapplicative f
  => Int -> Tree a -> f (Tree a) (U.S2 a (Tree a))
splitAtF = go
  where
    go !i (Bin _ x l r) = case compare i szl of
      LT -> second (second (\lr -> link x lr r)) (go i l)
      EQ -> U.bipure l (U.S2 x r)
      GT -> first (link x l) (go (i-szl-1) r)
      where
        szl = size l
    go _ Tip = errorOutOfBounds "Tree.splitAtF"
{-# INLINE splitAtF #-}

--------------
-- Transform
--------------

mapMaybeA :: Applicative f => (a -> f (Maybe b)) -> Tree a -> f (Tree b)
mapMaybeA f = foldSimple tip g
  where
    tip = pure Tip
    {-# INLINE tip #-}
    g _ x ml mr = (\h -> Ap.liftA3 h ml (f x) mr) $ \l my r ->
      case my of
        Nothing -> merge l r
        Just y -> link y l r
    {-# INLINE g #-}
{-# INLINE mapMaybeA #-}

mapEitherA
  :: Applicative f
  => (a -> f (Either b c)) -> Tree a -> f (U.S2 (Tree b) (Tree c))
mapEitherA f = foldSimple tip g
  where
    tip = pure (U.bipure Tip Tip)
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

zipWithStreamM :: Monad m => (a -> b -> m c) -> Tree a -> Stream b -> m (Tree c)
zipWithStreamM f t (Stream step s) = U.evalSStateT (foldSimple tip g t) s
  where
    tip = pure Tip
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
  :: Applicative f => (a -> f (b, c)) -> Tree a -> f (U.S2 (Tree b) (Tree c))
unzipWithA f = foldSimple tip g
  where
    tip = pure (U.S2 Tip Tip)
    {-# INLINE tip #-}
    g !sz x ml mr = (\h -> Ap.liftA3 h ml (f x) mr) $
      \(U.S2 l1 l2) (x1,x2) (U.S2 r1 r2) ->
        U.S2 (Bin sz x1 l1 r1) (Bin sz x2 l2 r2)
    {-# INLINE g #-}
{-# INLINE unzipWithA #-}

unzipWith3A
  :: Applicative f
  => (a -> f (b, c, d))
  -> Tree a
  -> f (U.S3 (Tree b) (Tree c) (Tree d))
unzipWith3A f = foldSimple tip g
  where
    tip = pure (U.S3 Tip Tip Tip)
    {-# INLINE tip #-}
    g !sz x ml mr = (\h -> Ap.liftA3 h ml (f x) mr) $
      \(U.S3 l1 l2 l3) (x1,x2,x3) (U.S3 r1 r2 r3) ->
        U.S3 (Bin sz x1 l1 r1) (Bin sz x2 l2 r2) (Bin sz x3 l3 r3)
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
link :: a -> Tree a -> Tree a -> Tree a
link !x Tip r = cons x r
link x l Tip = snoc l x
link x l@(Bin ls lx ll lr) r@(Bin rs rx rl rr)
  | delta*ls < rs = balanceL rx (linkL x ls l rl) rr
  | delta*rs < ls = balanceR lx ll (linkR x lr rs r)
  | otherwise     = Bin (1+ls+rs) x l r
{-# INLINE link #-}

linkL :: a -> Int -> Tree a -> Tree a -> Tree a
linkL !x !ls !l r = case r of
  Bin rs rx rl rr
    | delta*ls < rs -> balanceL rx (linkL x ls l rl) rr
    | otherwise     -> Bin (1+ls+rs) x l r
  Tip -> error "Tree.linkL: impossible"

linkR :: a -> Tree a -> Int -> Tree a -> Tree a
linkR !x l !rs !r = case l of
  Bin ls lx ll lr
    | delta*rs < ls -> balanceR lx ll (linkR x lr rs r)
    | otherwise     -> Bin (1+ls+rs) x l r
  Tip -> error "Tree.linkR: impossible"

-- O(log (n1 + n2)). Link two trees.
merge :: Tree a -> Tree a -> Tree a
merge Tip r = r
merge l Tip = l
merge l@(Bin ls lx ll lr) r@(Bin rs rx rl rr)
  | ls < rs = case unsnocSure lx ll lr of U.S2 l' mx -> link mx l' r
  | otherwise = case unconsSure rx rl rr of U.S2 mx r' -> link mx l r'
{-# INLINE merge #-}

-- O(log (n1 + n2)). Link two trees. Precondition: The trees must be balanced
-- wrt each other.
glue :: Tree a -> Tree a -> Tree a
glue Tip r = r
glue l Tip = l
glue l@(Bin ls lx ll lr) r@(Bin rs rx rl rr)
  | ls > rs = case unsnocSure lx ll lr of U.S2 l' m -> balanceR m l' r
  | otherwise = case unconsSure rx rl rr of U.S2 m r' -> balanceL m l r'
{-# INLINE glue #-}

-- Note [Balance]
-- ~~~~~~~~~~~~~~
-- The balancing code here is largely influenced by the implementation of
-- for the Set type in containers: https://hackage.haskell.org/package/containers
-- The linked papers in Data.Seqn.Seq describe the structure in greater detail.
--
-- To summarize:
-- * A tree is balanced if size(left child) < delta*size(right child) and vice
--   versa, which a special case for Tips. See `balanceOk` in `valid`, which is
--   used to check that balance holds in tests.
-- * Rebalancing involves rotations. The rotation can be single or double. The
--   constant `ratio` determines whether a double rotation is performed.

delta, ratio :: Int
delta = 3
ratio = 2

-- O(1). Restores balance with at most one right rotation. Precondition: One
-- right rotation must be enough to restore balance. This is the case when the
-- left tree might have been inserted to or the right tree deleted from.
balanceL :: a -> Tree a -> Tree a -> Tree a
balanceL !x l r = case r of
  Tip -> case l of
    Tip -> Bin 1 x Tip Tip
    Bin _ lx ll lr -> case lr of
      Tip -> case ll of
        Tip -> Bin 2 x l Tip
        Bin _ _ _ _ -> Bin 3 lx ll (Bin 1 x Tip Tip)
      Bin _ lrx _ _ -> case ll of
        Tip -> Bin 3 lrx (Bin 1 lx Tip Tip) (Bin 1 x Tip Tip)
        Bin _ _ _ _ -> Bin 4 lx ll (Bin 2 x lr Tip)
  Bin rs _ _ _ -> case l of
    Tip -> Bin (1+rs) x Tip r
    Bin ls lx ll lr
      | ls > delta*rs -> case (ll, lr) of
        (Bin lls _ _ _, Bin lrs lrx lrl lrr)
          | lrs < ratio*lls -> Bin (1+ls+rs) lx ll (Bin (1+rs+lrs) x lr r)
          | otherwise -> Bin (1+ls+rs) lrx (Bin (1+lls+size lrl) lx ll lrl) (Bin (1+rs+size lrr) x lrr r)
        _ -> error "Tree.balanceL: impossible"
      | otherwise -> Bin (1+ls+rs) x l r
{-# NOINLINE balanceL #-}

-- O(1). Restores balance with at most one left rotation. Precondition: One left
-- rotation must be enough to restore balance. This is the case when the right
-- tree might have been inserted to or the left tree deleted from.
balanceR :: a -> Tree a -> Tree a -> Tree a
balanceR !x l r = case l of
  Tip -> case r of
    Tip -> Bin 1 x Tip Tip
    Bin _ rx rl rr -> case rl of
      Tip -> case rr of
        Tip -> Bin 2 x Tip r
        Bin _ _ _ _ -> Bin 3 rx (Bin 1 x Tip Tip) rr
      Bin _ rlx _ _ -> case rr of
        Tip -> Bin 3 rlx (Bin 1 x Tip Tip) (Bin 1 rx Tip Tip)
        Bin _ _ _ _ -> Bin 4 rx (Bin 2 x Tip rl) rr
  Bin ls _ _ _ -> case r of
    Tip -> Bin (1+ls) x l Tip
    Bin rs rx rl rr
      | rs > delta*ls -> case (rl, rr) of
        (Bin rls rlx rll rlr, Bin rrs _ _ _)
          | rls < ratio*rrs -> Bin (1+ls+rs) rx (Bin (1+ls+rls) x l rl) rr
          | otherwise -> Bin (1+ls+rs) rlx (Bin (1+ls+size rll) x l rll) (Bin (1+rrs+size rlr) rx rlr rr)
        _ -> error "Tree.balanceR: impossible"
      | otherwise -> Bin (1+ls+rs) x l r
{-# NOINLINE balanceR #-}

------------
-- Testing
------------

valid :: Tree a -> Bool
valid s = balanceOk s && sizeOk s
  where
    balanceOk = \case
      Bin _ _ l r -> ok && balanceOk l && balanceOk r
        where
          ok = size l + size r <= 1 ||
               (size l <= delta * size r && size r <= delta * size l)
      Tip -> True

    sizeOk = \case
      Bin sz _ l r -> sizeOk l && sizeOk r && size l + size r + 1 == sz
      Tip -> True

debugShowsPrec :: Show a => Int -> Tree a -> ShowS
debugShowsPrec p = \case
  Bin sz x l r ->
    showParen (p > 10) $
      showString "Bin " .
      shows sz .
      showString " " .
      showsPrec 11 x .
      showString " " .
      debugShowsPrec 11 l .
      showString " " .
      debugShowsPrec 11 r
  Tip -> showString "Tip"
