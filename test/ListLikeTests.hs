{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module ListLikeTests where

import Prelude hiding ((<>), break, concatMap, drop, dropWhile, filter, liftA2, lookup, map, read, replicate, reverse, show, span, splitAt, take, takeWhile)
import qualified Control.Applicative as Ap
import Control.Monad.Trans.State (runState, state)
import Data.Bifunctor (Bifunctor(..))
import qualified Data.Either as Either
import qualified Data.Foldable as F
import qualified Data.Foldable.WithIndex as IFo
import qualified Data.Functor.WithIndex as IFu
import qualified Data.List as L
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import Data.Ord (comparing)
import qualified Data.Semigroup as Semigroup
import Test.Tasty
import Test.Tasty.QuickCheck hiding (generate)
import qualified Text.Read as Read
import qualified Text.Show as Show

import ListExtra
import TestUtil (MaybeBottom(..), ListLike(..), isBottom, eval)

-- This is defined for convenience. Not all tests require all of these
-- constraints.
type Common t =
  ( ListLike t
  , Eq t
  , Show t
  , Arbitrary t
  , Eq (E t)
  , Show (E t)
  , Arbitrary (E t)
  )
type FuncE t =
  ( CoArbitrary (E t)
  , Function (E t)
  )

listLike :: forall t. Common t => TestTree
listLike = testGroup "ListLike"
  [ testProperty "toList . fromList == id" $ \xs ->
      toL (fromL xs :: t) === xs
  , testProperty "fromList . toList == id" $ \(xs :: t) ->
      fromL (toL xs) === xs
  ]

--------------
-- Construct
--------------

empty :: ListLike t => t -> TestTree
empty e = testProperty "empty" $ null (toL e)

singleton :: Common t => (E t -> t) -> TestTree
singleton f = testProperty "singleton" $ \x -> toL (f x) === [x]

fromRevList :: Common t => ([E t] -> t) -> TestTree
fromRevList f = testProperty "fromRevList" $ \xs -> toL (f xs) === L.reverse xs

replicate :: Common t => (Int -> E t -> t) -> TestTree
replicate f = testProperty "replicate" $ \n x -> toL (f n x) === L.replicate n x

replicateA :: Common t => (forall f. Applicative f => Int -> f (E t) -> f t) -> TestTree
replicateA f = testProperty "replicateA" $ \xs ->
  let n = length xs in
  runState (fmap toL (f n (state (\(y:ys) -> (y,ys))))) xs === (xs, [])

generate :: Common t => (Int -> (Int -> E t) -> t) -> TestTree
generate f = testProperty "generate" $ \n fn ->
  toL (f n (applyFun fn)) === L.map (applyFun fn) [0..n-1]

generateA :: Common t => (forall f. Applicative f => Int -> (Int -> f (E t)) -> f t) -> TestTree
generateA f = testProperty "generateA" $ \xs ->
  let n = length xs in
  fmap toL (f n (\i -> ([i], xs !! i))) === ([0..n-1], xs)

unfoldr :: Common t => (([a] -> Maybe (a, [a])) -> [E t] -> t) -> TestTree
unfoldr f = testProperty "unfoldr" $ \xs -> toL (f L.uncons xs) === xs

unfoldl :: Common t => (([a] -> Maybe ([a], a)) -> [E t] -> t) -> TestTree
unfoldl f = testProperty "unfoldl" $ \xs -> toL (f unsnocL xs) === xs

unfoldrM :: Common t => (forall b m. Monad m => (b -> m (Maybe (E t, b))) -> b -> m t) -> TestTree
unfoldrM f = testProperty "unfoldrM" $ \xs ->
  fmap toL (f (\ys -> ([ys], L.uncons ys)) xs) === (L.tails xs, xs)

unfoldlM :: Common t => (forall b m. Monad m => (b -> m (Maybe (b, E t))) -> b -> m t) -> TestTree
unfoldlM f = testProperty "unfoldlM" $ \xs ->
  fmap toL (f (\ys -> ([ys], unsnocL ys)) xs) ===
  (L.reverse (L.inits xs), xs)

(<>) :: Common t => (t -> t -> t) -> TestTree
(<>) f = testProperty "<>" $ \xs ys ->
  toL (f xs ys) === toL xs Semigroup.<> toL ys

stimes :: Common t => (Int -> t -> t) -> TestTree
stimes f = testProperty "stimes" $ \(n :: Int) xs ->
  toL (f n xs) === Semigroup.stimes (max 0 n) (toL xs)

mconcat :: Common t => ([t] -> t) -> TestTree
mconcat f = testProperty "mconcat" $ \xss ->
  toL (f xss) === Monoid.mconcat (fmap toL xss)

concatMap :: Common t => (forall f a. Foldable f => (a -> t) -> f a -> t) -> TestTree
concatMap f = testProperty "concatMap" $ \fn (xs :: [Int]) ->
  toL (f (applyFun fn) xs) === L.concatMap (toL . applyFun fn) xs

read :: forall t. (Common t, Read t, Read (E t)) => TestTree
read = testProperty "read" $ \s -> case s of
  Left str ->
    fmap toL (Read.readMaybe str :: Maybe t) === Read.readMaybe str
  Right (xs :: [E t]) ->
    fmap toL (Read.readMaybe (Show.show xs) :: Maybe t) === Just xs

------------
-- Convert
------------

toRevList :: Common t => (t -> [E t]) -> TestTree
toRevList f = testProperty "toRevList" $ \xs -> f xs === L.reverse (toL xs)

show :: forall t. Common t => TestTree
show = testProperty "show" $ \(xs :: t) -> Show.show xs === Show.show (toL xs)

---------
-- Fold
---------

-- Note: For the foldr/foldl family, strictness is checked in addition to
-- correctness. For the foldMap family strictness is /not checked/ because
-- these usually follow the internal structure of the Foldable. This results in
-- different strictness in different places compared to a list.

foldMap :: Common t => (forall m. Monoid m => (E t -> m) -> t -> m) -> TestTree
foldMap f = testProperty "foldMap" $ \xs -> f (:[]) xs === toL xs

foldMap' :: Common t => (forall m. Monoid m => (E t -> m) -> t -> m) -> TestTree
foldMap' f = testProperty "foldMap'" $ \xs -> f (:[]) xs === toL xs

foldr :: (Common t, FuncE t) => (forall b. (E t -> b -> b) -> b -> t -> b) -> TestTree
foldr f = testProperty "foldr" $ \xs fn (z0 :: Int) ->
  let g x z = case applyFun fn x of
        Left h -> applyFun h z
        Right Bottom -> error "bottom"
        Right (NotBottom z') -> z'
  in testFold (f g z0) (L.foldr g z0) xs

foldl' :: (Common t, FuncE t) => (forall b. (b -> E t -> b) -> b -> t -> b) -> TestTree
foldl' f = testProperty "foldl'" $ \xs fn (z0 :: Int) ->
  let g z x = case applyFun fn x of
        Left h -> applyFun h z
        Right Bottom -> error "bottom"
        Right (NotBottom z') -> z'
  in testFold (f g z0) (L.foldl' g z0) xs

foldr' :: (Common t, FuncE t) => (forall b. (E t -> b -> b) -> b -> t -> b) -> TestTree
foldr' f = testProperty "foldr'" $ \xs fn (z0 :: Int) ->
  let g x z = case applyFun fn x of
        Left h -> applyFun h z
        Right Bottom -> error "bottom"
        Right (NotBottom z') -> z'
  in testFold (f g z0) (F.foldr' g z0) xs

foldl :: (Common t, FuncE t) => (forall b. (b -> E t -> b) -> b -> t -> b) -> TestTree
foldl f = testProperty "foldl" $ \xs fn (z0 :: Int) ->
  let g z x = case applyFun fn x of
        Left h -> applyFun h z
        Right Bottom -> error "bottom"
        Right (NotBottom z') -> z'
  in testFold (f g z0) (L.foldl g z0) xs

ifoldMap :: Common t => (forall m. Monoid m => (Int -> E t -> m) -> t -> m) -> TestTree
ifoldMap f = testProperty "ifoldMap" $ \xs ->
  f (\i x -> [(i,x)]) xs === L.zip [0..] (toL xs)

ifoldMap' :: Common t => (forall m. Monoid m => (Int -> E t -> m) -> t -> m) -> TestTree
ifoldMap' f = testProperty "ifoldMap'" $ \xs ->
  f (\i x -> [(i,x)]) xs === L.zip [0..] (toL xs)

ifoldr :: (Common t, FuncE t) => (forall b. (Int -> E t -> b -> b) -> b -> t -> b) -> TestTree
ifoldr f = testProperty "ifoldr" $ \xs fn (z0 :: Int) ->
  let g i x z = case applyFun2 fn i x of
        Left h -> applyFun h z
        Right Bottom -> error "bottom"
        Right (NotBottom z') -> z'
  in testFold (f g z0) (IFo.ifoldr g z0) xs

ifoldl :: (Common t, FuncE t) => (forall b. (Int -> b -> E t -> b) -> b -> t -> b) -> TestTree
ifoldl f = testProperty "ifoldl" $ \xs fn (z0 :: Int) ->
  let g i z x = case applyFun2 fn i x of
        Left h -> applyFun h z
        Right Bottom -> error "bottom"
        Right (NotBottom z') -> z'
  in testFold (f g z0) (IFo.ifoldl g z0) xs

ifoldr' :: (Common t, FuncE t) => (forall b. (Int -> E t -> b -> b) -> b -> t -> b) -> TestTree
ifoldr' f = testProperty "ifoldr'" $ \xs fn (z0 :: Int) ->
  let g i x z = case applyFun2 fn i x of
        Left h -> applyFun h z
        Right Bottom -> error "bottom"
        Right (NotBottom z') -> z'
  in testFold (f g z0) (IFo.ifoldr' g z0) xs

ifoldl' :: (Common t, FuncE t) => (forall b. (Int -> b -> E t -> b) -> b -> t -> b) -> TestTree
ifoldl' f = testProperty "ifoldl" $ \xs fn (z0 :: Int) ->
  let g i z x = case applyFun2 fn i x of
        Left h -> applyFun h z
        Right Bottom -> error "bottom"
        Right (NotBottom z') -> z'
  in testFold (f g z0) (IFo.ifoldl' g z0) xs

testFold
  :: (Common t, Eq a, Show a) => (t -> a) -> ([E t] -> a) -> t -> Property
testFold f1 f2 xs = ioProperty $ do
  r1 <- eval (f1 xs)
  r2 <- eval (f2 (toL xs))
  pure $
    classify (isBottom r2) "bottom" $
      r1 === r2

----------
-- Index
----------

lookup :: Common t => (Int -> t -> Maybe (E t)) -> TestTree
lookup f = testProperty "lookup" $ \i xs -> f i xs === lookupL i (toL xs)

index :: Common t => (Int -> t -> E t) -> TestTree
index f = testProperty "index" $ \i xs ->
  0 <= i && i < length (toL xs) ==>
    f i xs === toL xs !! i

update :: Common t => (Int -> E t -> t -> t) -> TestTree
update f = testProperty "update" $ \i x xs ->
  toL (f i x xs) === adjustL (\_ -> x) i (toL xs)

adjust :: (Common t, FuncE t) => ((E t -> E t) -> Int -> t -> t) -> TestTree
adjust f = testProperty "adjust" $ \fn i xs ->
  toL (f (applyFun fn) i xs) === adjustL (applyFun fn) i (toL xs)

insertAt :: Common t => (Int -> E t -> t -> t) -> TestTree
insertAt f = testProperty "insertAt" $ \i x xs ->
  toL (f i x xs) === insertAtL i x (toL xs)

deleteAt :: Common t => (Int -> t -> t) -> TestTree
deleteAt f = testProperty "deleteAt" $ \i xs ->
  toL (f i xs) === deleteAtL i (toL xs)

----------
-- Slice
----------

cons :: Common t => (E t -> t -> t) -> TestTree
cons f = testProperty "cons" $ \x xs -> toL (f x xs) === x : toL xs

snoc :: Common t => (t -> E t -> t) -> TestTree
snoc f = testProperty "snoc" $ \xs x -> toL (f xs x) === toL xs ++ [x]

uncons :: Common t => (t -> Maybe (E t, t)) -> TestTree
uncons f = testProperty "uncons" $ \xs ->
  fmap (fmap toL) (f xs) === L.uncons (toL xs)

unsnoc :: Common t => (t -> Maybe (t, E t)) -> TestTree
unsnoc f = testProperty "unsnoc" $ \xs ->
  fmap (first toL) (f xs) === unsnocL (toL xs)

take :: Common t => (Int -> t -> t) -> TestTree
take f = testProperty "take" $ \n xs -> toL (f n xs) === L.take n (toL xs)

drop :: Common t => (Int -> t -> t) -> TestTree
drop f = testProperty "drop" $ \n xs -> toL (f n xs) === L.drop n (toL xs)

slice :: Common t => ((Int, Int) -> t -> t) -> TestTree
slice f = testProperty "slice" $ \ij xs -> toL (f ij xs) === sliceL ij (toL xs)

splitAt :: Common t => (Int -> t -> (t,t)) -> TestTree
splitAt f = testProperty "splitAt" $ \n xs ->
  bimap toL toL (f n xs) === L.splitAt n (toL xs)

takeEnd :: Common t => (Int -> t -> t) -> TestTree
takeEnd f = testProperty "takeEnd" $ \n xs ->
  toL (f n xs) === takeEndL n (toL xs)

dropEnd :: Common t => (Int -> t -> t) -> TestTree
dropEnd f = testProperty "dropEnd" $ \n xs ->
  toL (f n xs) === dropEndL n (toL xs)

splitAtEnd :: Common t => (Int -> t -> (t,t)) -> TestTree
splitAtEnd f = testProperty "splitAtEnd" $ \n xs ->
  bimap toL toL (f n xs) ===
  (dropEndL n (toL xs), takeEndL n (toL xs))

tails :: (Common t, ListLike t2, t ~ E t2) => (t -> t2) -> TestTree
tails f = testProperty "tails" $ \xs ->
  L.map toL (toL (f xs)) === L.tails (toL xs)

inits :: (Common t, ListLike t2, t ~ E t2) => (t -> t2) -> TestTree
inits f = testProperty "inits" $ \xs ->
  L.map toL (toL (f xs)) === L.inits (toL xs)

chunksOf :: (Common t, ListLike t2, t ~ E t2) => (Int -> t -> t2) -> TestTree
chunksOf f = testProperty "chunksOf" $ \c xs ->
  L.map toL (toL (f c xs)) === chunksOfL c (toL xs)

-----------
-- Filter
-----------

filter :: (Common t, FuncE t) => ((E t -> Bool) -> t -> t) -> TestTree
filter f = testProperty "filter" $ \fn xs ->
  toL (f (applyFun fn) xs) === L.filter (applyFun fn) (toL xs)

mapMaybe :: (Common t1, FuncE t1, Common t2) => ((E t1 -> Maybe (E t2)) -> t1 -> t2) -> TestTree
mapMaybe f = testProperty "mapMaybe" $ \fn xs ->
  toL (f (applyFun fn) xs) ===
  Maybe.mapMaybe (applyFun fn) (toL xs)

mapEither :: (Common t1, FuncE t1, Common t2, Common t3) => ((E t1 -> Either (E t2) (E t3)) -> t1 -> (t2, t3)) -> TestTree
mapEither f = testProperty "mapEither" $ \fn xs ->
  bimap toL toL (f (applyFun fn) xs) ===
  Either.partitionEithers (fmap (applyFun fn) (toL xs))

filterM :: (Common t, FuncE t) => (forall m. Monad m => (E t -> m Bool) -> t -> m t) -> TestTree
filterM f = testProperty "filter" $ \fn xs ->
  fmap toL (f (\x -> ([x], applyFun fn x)) xs) ===
  (toL xs, L.filter (applyFun fn) (toL xs))

mapMaybeM :: (Common t1, FuncE t1, Common t2) => (forall m. Monad m => (E t1 -> m (Maybe (E t2))) -> t1 -> m t2) -> TestTree
mapMaybeM f = testProperty "mapMaybeM" $ \fn xs ->
  fmap toL (f (\x -> ([x], applyFun fn x)) xs) ===
  (toL xs, Maybe.mapMaybe (applyFun fn) (toL xs))

mapEitherM :: (Common t1, FuncE t1, Common t2, Common t3) => (forall m. Monad m => (E t1 -> m (Either (E t2) (E t3))) -> t1 -> m (t2, t3)) -> TestTree
mapEitherM f = testProperty "mapEitherM" $ \fn xs ->
  fmap (bimap toL toL) (f (\x -> ([x], applyFun fn x)) xs) ===
  (toL xs, Either.partitionEithers (fmap (applyFun fn) (toL xs)))

takeWhile :: (Common t, FuncE t) => ((E t -> Bool) -> t -> t) -> TestTree
takeWhile f = testProperty "takeWhile" $ \fn xs ->
  toL (f (applyFun fn) xs) === L.takeWhile (applyFun fn) (toL xs)

dropWhile :: (Common t, FuncE t) => ((E t -> Bool) -> t -> t) -> TestTree
dropWhile f = testProperty "dropWhile" $ \fn xs ->
  toL (f (applyFun fn) xs) === L.dropWhile (applyFun fn) (toL xs)

span :: (Common t, FuncE t) => ((E t -> Bool) -> t -> (t,t)) -> TestTree
span f = testProperty "span" $ \fn xs ->
  bimap toL toL (f (applyFun fn) xs) === L.span (applyFun fn) (toL xs)

break :: (Common t, FuncE t) => ((E t -> Bool) -> t -> (t,t)) -> TestTree
break f = testProperty "break" $ \fn xs ->
  bimap toL toL (f (applyFun fn) xs) === L.break (applyFun fn) (toL xs)

takeWhileEnd :: (Common t, FuncE t) => ((E t -> Bool) -> t -> t) -> TestTree
takeWhileEnd f = testProperty "takeWhileEnd" $ \fn xs ->
  toL (f (applyFun fn) xs) ===
  snd (spanEndL (applyFun fn) (toL xs))

dropWhileEnd :: (Common t, FuncE t) => ((E t -> Bool) -> t -> t) -> TestTree
dropWhileEnd f = testProperty "dropWhileEnd" $ \fn xs ->
  toL (f (applyFun fn) xs) ===
  fst (spanEndL (applyFun fn) (toL xs))

spanEnd :: (Common t, FuncE t) => ((E t -> Bool) -> t -> (t,t)) -> TestTree
spanEnd f = testProperty "spanEnd" $ \fn xs ->
  bimap toL toL (f (applyFun fn) xs) ===
  spanEndL (applyFun fn) (toL xs)

breakEnd :: (Common t, FuncE t) => ((E t -> Bool) -> t -> (t,t)) -> TestTree
breakEnd f = testProperty "breakEnd" $ \fn xs ->
  bimap toL toL (f (applyFun fn) xs) ===
  spanEndL (not . applyFun fn) (toL xs)

--------------
-- Transform
--------------

map :: (Common t1, FuncE t1, Common t2) => ((E t1 -> E t2) -> t1 -> t2) -> TestTree
map f = testProperty "map" $ \fn xs ->
  toL (f (applyFun fn) xs) === L.map (applyFun fn) (toL xs)

liftA2 :: (Common t1, FuncE t1, Common t2, FuncE t2, Common t3) => ((E t1 -> E t2 -> E t3) -> t1 -> t2 -> t3) -> TestTree
liftA2 f = testProperty "liftA2" $
  \fn xs ys ->
    toL (f (applyFun2 fn) xs ys) ===
    Ap.liftA2 (applyFun2 fn) (toL xs) (toL ys)

(<*) :: (Common t1, Common t2) => (t1 -> t2 -> t1) -> TestTree
(<*) f = testProperty "<*" $ \xs ys ->
  toL (f xs ys) === (toL xs Ap.<* toL ys)

(*>) :: (Common t1, Common t2) => (t1 -> t2 -> t2) -> TestTree
(*>) f = testProperty "*>" $ \xs ys ->
  toL (f xs ys) === (toL xs Ap.*> toL ys)

bind :: (Common t1, FuncE t1, Common t2) => (t1 -> (E t1 -> t2) -> t2) -> TestTree
bind f = testProperty ">>=" $ \xs fn ->
  toL (f xs (applyFun fn)) === (toL xs >>= toL . applyFun fn)

traverse :: (Common t1, FuncE t1, Common t2) => (forall f. Applicative f => (E t1 -> f (E t2)) -> t1 -> f t2) -> TestTree
traverse f = testProperty "traverse" $ \fn xs ->
  fmap toL (f (\x -> ([x], applyFun fn x)) xs) ===
  (toL xs, fmap (applyFun fn) (toL xs))

imap :: (Common t1, FuncE t1, Common t2) => ((Int -> E t1 -> E t2) -> t1 -> t2) -> TestTree
imap f = testProperty "imap" $ \fn xs ->
  toL (f (applyFun2 fn) xs) === IFu.imap (applyFun2 fn) (toL xs)

itraverse :: (Common t1, FuncE t1, Common t2) => (forall f. Applicative f => (Int -> E t1 -> f (E t2)) -> t1 -> f t2) -> TestTree
itraverse f = testProperty "itraverse" $ \fn xs ->
  let ixs = L.zip [0..] (toL xs) in
  fmap toL (f (\i x -> ([(i,x)], applyFun2 fn i x)) xs) ===
  (ixs, L.map (uncurry (applyFun2 fn)) ixs)

reverse :: Common t => (t -> t) -> TestTree
reverse f = testProperty "reverse" $ \xs ->
  toL (f xs) === L.reverse (toL xs)

intersperse :: Common t => (E t -> t -> t) -> TestTree
intersperse f = testProperty "intersperse" $ \x xs ->
  toL (f x xs) === L.intersperse x (toL xs)

scanl :: (Common t1, FuncE t1, Common t2, FuncE t2) => ((E t2 -> E t1 -> E t2) -> E t2 -> t1 -> t2) -> TestTree
scanl f = testProperty "scanl" $ \fn z xs ->
  toL (f (applyFun2 fn) z xs) === L.scanl' (applyFun2 fn) z (toL xs)

scanr :: (Common t1, FuncE t1, Common t2, FuncE t2) => ((E t1 -> E t2 -> E t2) -> E t2 -> t1 -> t2) -> TestTree
scanr f = testProperty "scanr" $ \fn z xs ->
  toL (f (applyFun2 fn) z xs) === L.scanr (applyFun2 fn) z (toL xs)

sort :: (Common t, Ord (E t)) => (t -> t) -> TestTree
sort f = testProperty "sort" $ \xs ->
  toL (f xs) === L.sort (toL xs)

sortBy :: forall t. (Common t, FuncE t) => ((E t -> E t -> Ordering) -> t -> t) -> TestTree
sortBy f = testProperty "sortBy" $ \(fn :: Fun (E t) Int) xs ->
  toL (f (comparing (applyFun fn)) xs) ===
  L.sortBy (comparing (applyFun fn)) (toL xs)

--------------------
-- Search and test
--------------------

eq :: Common t => (t -> t -> Bool) -> TestTree
eq f = testProperty "==" $ \xs ys ->
  let tru = toL xs == toL ys in
  classify (tru && not (null (toL xs))) "non-trivial yes" $
    f xs ys === tru

cmp :: (Common t, Ord (E t)) => (t -> t -> Ordering) -> TestTree
cmp f = testProperty "compare" $ \xs ys ->
  let res = compare (toL xs) (toL ys) in
  collect res $
    f xs ys === res

findEnd :: (Common t, FuncE t) => ((E t -> Bool) -> t -> Maybe (E t)) -> TestTree
findEnd f = testProperty "findEnd" $ \fn xs ->
  f (applyFun fn) xs === L.find (applyFun fn) (L.reverse (toL xs))

findIndex :: (Common t, FuncE t) => ((E t -> Bool) -> t -> Maybe Int) -> TestTree
findIndex f = testProperty "findIndex" $ \fn xs ->
  f (applyFun fn) xs === L.findIndex (applyFun fn) (toL xs)

findIndexEnd :: (Common t, FuncE t) => ((E t -> Bool) -> t -> Maybe Int) -> TestTree
findIndexEnd f = testProperty "findIndexEnd" $ \fn xs ->
  f (applyFun fn) xs ===
  fmap snd (unsnocL (L.findIndices (applyFun fn) (toL xs)))

infixIndices :: Common t => (t -> t -> [Int]) -> TestTree
infixIndices f = testProperty "infixIndices" $ \xs ys ->
  let res = infixIndicesL (toL xs) (toL ys) in
  classify (not (null res) && not (null (toL xs))) "non-trivial yes" $
    f xs ys === res

binarySearchFind :: (Common t, Ord (E t)) => ((E t -> Ordering) -> t -> Maybe (E t)) -> TestTree
binarySearchFind f = testProperty "binarySearchFind" $ \(Sorted xs) x ->
  f (`compare` x) (fromL xs) === L.find (x==) xs

isPrefixOf :: Common t => (t -> t -> Bool) -> TestTree
isPrefixOf f = testProperty "isPrefixOf" $ \xs ys ->
  let tru = toL xs `L.isPrefixOf` toL ys in
  classify (tru && not (null (toL xs))) "non-trivial yes" $
    f xs ys === tru

isSuffixOf :: Common t => (t -> t -> Bool) -> TestTree
isSuffixOf f = testProperty "isSuffixOf" $ \xs ys ->
  let tru = toL xs `L.isSuffixOf` toL ys in
  classify (tru && not (null (toL xs))) "non-trivial yes" $
    f xs ys === tru

isInfixOf :: Common t => (t -> t -> Bool) -> TestTree
isInfixOf f = testProperty "isInfixOf" $ \xs ys ->
  let tru = toL xs `L.isInfixOf` toL ys in
  classify (tru && not (null (toL xs))) "non-trivial yes" $
    f xs ys === tru

isSubsequenceOf :: Common t => (t -> t -> Bool) -> TestTree
isSubsequenceOf f = testProperty "isSubsequenceOf" $ \xs ys ->
  let tru = toL xs `L.isSubsequenceOf` toL ys in
  classify (tru && not (null (toL xs))) "non-trivial yes" $
        f xs ys === tru

------------------
-- Zip and unzip
------------------

zip :: (Common t1, Common t2, Common t3, E t3 ~ (E t1, E t2)) => (t1 -> t2 -> t3) -> TestTree
zip f = testProperty "zip" $ \xs ys -> toL (f xs ys) === L.zip (toL xs) (toL ys)

zip3 :: (Common t1, Common t2, Common t3, Common t4, E t4 ~ (E t1, E t2, E t3)) => (t1 -> t2 -> t3 -> t4) -> TestTree
zip3 f = testProperty "zip3" $ \xs ys zs ->
  toL (f xs ys zs) === L.zip3 (toL xs) (toL ys) (toL zs)

zipWith :: (Common t1, FuncE t1, Common t2, FuncE t2, Common t3) => ((E t1 -> E t2 -> E t3) -> t1 -> t2 -> t3) -> TestTree
zipWith f = testProperty "zipWith" $ \fn xs ys ->
  toL (f (applyFun2 fn) xs ys) ===
  L.zipWith (applyFun2 fn) (toL xs) (toL ys)

zipWith3 :: (Common t1, FuncE t1, Common t2, FuncE t2, Common t3, FuncE t3, Common t4) => ((E t1 -> E t2 -> E t3 -> E t4) -> t1 -> t2 -> t3 -> t4) -> TestTree
zipWith3 f = testProperty "zipWith3" $ \fn xs ys zs ->
  toL (f (applyFun3 fn) xs ys zs) ===
  L.zipWith3 (applyFun3 fn) (toL xs) (toL ys) (toL zs)

zipWithM :: (Common t1, FuncE t1, Common t2, FuncE t2, Common t3) => (forall m. Monad m => (E t1 -> E t2 -> m (E t3)) -> t1 -> t2 -> m t3) -> TestTree
zipWithM f = testProperty "zipWith" $ \fn xs ys ->
  fmap toL (f (\x y -> ([(x,y)], applyFun2 fn x y)) xs ys) ===
  (L.zip (toL xs) (toL ys), L.zipWith (applyFun2 fn) (toL xs) (toL ys))

zipWith3M :: (Common t1, FuncE t1, Common t2, FuncE t2, Common t3, FuncE t3, Common t4) => (forall m. Monad m => (E t1 -> E t2 -> E t3 -> m (E t4)) -> t1 -> t2 -> t3 -> m t4) -> TestTree
zipWith3M f = testProperty "zipWith" $ \fn xs ys zs ->
  fmap toL (f (\x y z -> ([(x,y,z)], applyFun3 fn x y z)) xs ys zs) ===
  ( L.zip3 (toL xs) (toL ys) (toL zs)
  , L.zipWith3 (applyFun3 fn) (toL xs) (toL ys) (toL zs)
  )

unzip :: (Common t1, Common t2, Common t3, E t1 ~ (E t2, E t3)) => (t1 -> (t2, t3)) -> TestTree
unzip f = testProperty "unzip" $ \xs ->
  bimap toL toL (f xs) === L.unzip (toL xs)

unzip3 :: (Common t1, Common t2, Common t3, Common t4, E t1 ~ (E t2, E t3, E t4)) => (t1 -> (t2, t3, t4)) -> TestTree
unzip3 f = testProperty "unzip3" $ \xs ->
  (\(a,b,c) -> (toL a, toL b, toL c)) (f xs) === L.unzip3 (toL xs)

unzipWith :: (Common t1, FuncE t1, Common t2, Common t3) => ((E t1 -> (E t2, E t3)) -> t1 -> (t2, t3)) -> TestTree
unzipWith f = testProperty "unzipWith" $ \fn xs ->
  bimap toL toL (f (applyFun fn) xs) ===
  L.unzip (fmap (applyFun fn) (toL xs))

unzipWith3 :: (Common t1, FuncE t1, Common t2, Common t3, Common t4) => ((E t1 -> (E t2, E t3, E t4)) -> t1 -> (t2, t3, t4)) -> TestTree
unzipWith3 f = testProperty "unzipWith3" $ \fn xs ->
  (\(as,bs,cs) -> (toL as, toL bs, toL cs)) (f (applyFun fn) xs) ===
  L.unzip3 (fmap (applyFun fn) (toL xs))
