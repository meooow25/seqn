{-# OPTIONS_GHC -Wno-orphans #-} -- Arbitrary instances
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module MSeq (mseqTests) where

import Prelude hiding (break, concatMap, drop, dropWhile, filter, liftA2, lookup, map, replicate, reverse, splitAt, scanl, scanr, span, take, takeWhile, traverse, zipWith, zipWith3)
import Data.Coerce (coerce)
import qualified Control.Applicative as Ap
import qualified Data.Foldable as F
import qualified Data.Foldable.WithIndex as IFo
import Data.Functor.Identity (Identity(..))
import qualified Data.List as L
import Data.Monoid (Sum(..))
import Data.Proxy (Proxy(..))
import Data.Semigroup (stimes)
import Test.Tasty
import Test.Tasty.QuickCheck hiding (generate)
import qualified Test.QuickCheck.Classes.Base as QLaws

import Data.Seqn.MSeq
import qualified Data.Seqn.Internal.MSeq as MSeqInternal
import qualified Data.Seqn.Internal.MTree as MTreeInternal
import ListExtra (sliceL, unsnocL)
import qualified ListLikeTests as LL
import TestUtil ((.:), ListLike(..), Sqrt1(..), tastyLaws)

mseqTests :: TestTree
mseqTests = testGroup "Data.Seqn.MSeq"
  [ testGroup "properties"
    [
      LL.listLike @(MSeq A)

      -- Measured queries
    , testProperty "summaryMay" $ \(xs :: MSeq A) ->
        summaryMay xs === foldMap (Just . measure) (F.toList xs)
    , testProperty "summary" $ \(xs :: MSeq D) ->
        summary xs === foldMap measure (F.toList xs)
    , testProperty "sliceSummaryMay" $ \(xs :: MSeq A) lu ->
        sliceSummaryMay lu xs ===
        foldMap (Just . measure) (sliceL lu (F.toList xs))
    , testProperty "sliceSummary" $ \(xs :: MSeq D) lu ->
        sliceSummary lu xs ===
        foldMap measure (sliceL lu (F.toList xs))
    , testProperty "foldlSliceSummaryComponents mempty (<>) == sliceSummary" $
        \(xs :: MSeq D) lu ->
          foldlSliceSummaryComponents (<>) mempty lu xs ===
          foldMap measure (sliceL lu (F.toList xs))
    , testProperty "foldlSliceSummaryComponents countLessThanInSlice" $
        \(xs :: MSeq (MultisetElem Int)) k lu ->
          let countLessThanInSlice =
                foldlSliceSummaryComponents
                  (\z ys -> z + countLessThanMultiset k ys)
                  0
          in
            countLessThanInSlice lu xs ===
            (length . L.filter ((<k) . unMultisetElem) . sliceL lu . F.toList) xs
    , testProperty "foldlSliceSummaryComponents O(log n)" $
        \(xs :: MSeq D) lu@(l,u) ->
          let res = foldlSliceSummaryComponents (\z _ -> z+1) 0 lu xs
              d = fromIntegral (max 1 (u-l+2)) :: Double
              lim = 4 * ceiling (logBase 2 d) :: Integer
              -- 4*log because 2*log from each side of the root.
              -- 2*log because subtree root + one child for each level.
              -- Could find a tighter bound but this is fine for testing.
          in counterexample ("res=" ++ show res ++ ", lim=" ++ show lim) $
               res <= lim
    , testProperty "binarySearchPrefix" $ \(xs :: MSeq SumElem) y ->
        let p = (>=y) . getSum
            xs' = F.toList xs
            iws =
              [ (i, foldMap measure (L.take (i+1) xs'))
              | i <- [0 .. length xs - 1]
              ]
            lastFalse = snd <$> unsnocL [i | (i,w) <- iws, not (p w)]
            firstTrue = fst <$> L.uncons [i | (i,w) <- iws, p w]
        in binarySearchPrefix p xs === (lastFalse, firstTrue)
    , testProperty "binarySearchSuffix" $ \(xs :: MSeq SumElem) y ->
        let p = (>=y) . getSum
            xs' = F.toList xs
            iws =
              [ (i, foldMap measure (L.drop i xs'))
              | i <- [0 .. length xs - 1]
              ]
            lastTrue = snd <$> unsnocL [i | (i,w) <- iws, p w]
            firstFalse = fst <$> L.uncons [i | (i,w) <- iws, not (p w)]
        in binarySearchSuffix p xs === (lastTrue, firstFalse)

      -- Construct
    , LL.fromRevList @(MSeq A) fromRevList
    , LL.empty @(MSeq A) empty
    , LL.singleton @(MSeq A) singleton
    , LL.replicate @(MSeq A) replicate
    , LL.replicateA @(MSeq A) replicateA
    , LL.generate @(MSeq A) generate
    , LL.generateA @(MSeq A) generateA
    , LL.unfoldr @(MSeq A) unfoldr
    , LL.unfoldl @(MSeq A) unfoldl
    , LL.unfoldrM @(MSeq A) unfoldrM
    , LL.unfoldlM @(MSeq A) unfoldlM
    , (LL.<>) @(MSeq A) (<>)
    , LL.stimes @(MSeq A) stimes
    , LL.mconcat @(MSeq A) mconcat
    , LL.concatMap @(MSeq A) concatMap
    , testProperty "mfix" $ \n ->
        F.toList (mkMfix n) ===
        fmap (LI . L.replicate 10 . fromIntegral) [0 .. n-1]
    , LL.read @(MSeq A)

      -- Convert
    , LL.toRevList @(MSeq A) toRevList
    , LL.show @(MSeq A)

      -- Fold
    , LL.foldMap @(MSeq A) foldMap
    , LL.foldMap' @(MSeq A) F.foldMap'
    , LL.foldr @(MSeq A) foldr
    , LL.foldl @(MSeq A) F.foldl
    , LL.foldl' @(MSeq A) F.foldl'
    , LL.foldr' @(MSeq A) F.foldr'
    , LL.ifoldMap @(MSeq A) IFo.ifoldMap
    , LL.ifoldr @(MSeq A) IFo.ifoldr
    , LL.ifoldl @(MSeq A) IFo.ifoldl
    , LL.ifoldr' @(MSeq A) IFo.ifoldr'
    , LL.ifoldl' @(MSeq A) IFo.ifoldl'

      -- Index
    , LL.lookup @(MSeq A) lookup
    , LL.index @(MSeq A) index
    , LL.update @(MSeq A) update
    , LL.adjust @(MSeq A) adjust
    , LL.insertAt @(MSeq A) insertAt
    , LL.deleteAt @(MSeq A) deleteAt

      -- Slice
    , LL.cons @(MSeq A) cons
    , LL.snoc @(MSeq A) snoc
    , LL.uncons @(MSeq A) uncons
    , LL.unsnoc @(MSeq A) unsnoc
    , LL.take @(MSeq A) take
    , LL.drop @(MSeq A) drop
    , LL.slice @(MSeq A) slice
    , LL.splitAt @(MSeq A) splitAt
    , LL.takeEnd @(MSeq A) takeEnd
    , LL.dropEnd @(MSeq A) dropEnd
    , LL.splitAtEnd @(MSeq A) splitAtEnd

      -- Filter
    , LL.filter @(MSeq A) filter
    , LL.mapMaybe @(MSeq A) @(MSeq B) mapMaybe
    , LL.mapEither @(MSeq A) @(MSeq B) @(MSeq C) mapEither
    , LL.filterM @(MSeq A) filterA
    , LL.mapMaybeM @(MSeq A) @(MSeq B) mapMaybeA
    , LL.mapEitherM @(MSeq A) @(MSeq B) @(MSeq C) mapEitherA
    , LL.takeWhile @(MSeq A) takeWhile
    , LL.dropWhile @(MSeq A) dropWhile
    , LL.span @(MSeq A) span
    , LL.break @(MSeq A) break
    , LL.takeWhileEnd @(MSeq A) takeWhileEnd
    , LL.dropWhileEnd @(MSeq A) dropWhileEnd
    , LL.spanEnd @(MSeq A) spanEnd
    , LL.breakEnd @(MSeq A) breakEnd

      -- Transform
    , LL.map @(MSeq A) @(MSeq B) map
    , LL.liftA2 @(Sqrt1 MSeq A) @(Sqrt1 MSeq B) @(Sqrt1 MSeq C) (coerce liftA2)
    , LL.traverse @(MSeq A) @(MSeq B) traverse
    , LL.imap @(MSeq A) @(MSeq B) imap
    , LL.itraverse @(MSeq A) @(MSeq B) itraverse
    , LL.reverse @(MSeq A) reverse
    , LL.intersperse @(MSeq A) intersperse
    , LL.scanl @(MSeq A) @(MSeq B) scanl
    , LL.scanr @(MSeq A) @(MSeq B) scanr
    , LL.sort @(MSeq A) sort
    , LL.sortBy @(MSeq A) sortBy

      -- Search and test
    , LL.eq @(MSeq A) (==)
    , LL.cmp @(MSeq A) compare
    , LL.findEnd @(MSeq A) findEnd
    , LL.findIndex @(MSeq A) findIndex
    , LL.findIndexEnd @(MSeq A) findIndexEnd
    , LL.infixIndices @(MSeq A) infixIndices
    , LL.binarySearchFind @(MSeq A) binarySearchFind
    , LL.isPrefixOf @(MSeq A) isPrefixOf
    , LL.isSuffixOf @(MSeq A) isSuffixOf
    , LL.isInfixOf @(MSeq A) isInfixOf
    , LL.isSubsequenceOf @(MSeq A) isSubsequenceOf

      -- Zip and unzip
    , LL.zipWith @(MSeq A) @(MSeq B) @(MSeq C) zipWith
    , LL.zipWith3 @(MSeq A) @(MSeq B) @(MSeq C) @(MSeq A) zipWith3
    , LL.zipWithM @(MSeq A) @(MSeq B) @(MSeq C) zipWithM
    , LL.zipWith3M @(MSeq A) @(MSeq B) @(MSeq C) @(MSeq A) zipWith3M
    , LL.unzipWith @(MSeq A) @(MSeq B) @(MSeq C) unzipWith
    , LL.unzipWith3 @(MSeq A) @(MSeq B) @(MSeq C) @(MSeq A) unzipWith3
    ]

  , testGroup "valid"
    [
      -- Arbitrary
      testProperty "arbitrary" $ id @(MSeq A)

      -- Construct
    , testProperty "fromList" $ fromList @A
    , testProperty "fromRevList" $ fromRevList @A
    , testProperty "replicate" $ replicate @A
    , testProperty "generate" $ \n -> generate n . applyFun @Int @A
    , testProperty "unfoldr" $ unfoldr (L.uncons @A)
    , testProperty "unfoldl" $ unfoldl (unsnocL @A)
    , testProperty "<>" $ (<>) @(MSeq A)
    , testProperty "stimes" $ stimes @(MSeq A) @Int
    , testProperty "mconcat" $ mconcat @(MSeq A)
    , testProperty "concatMap []" $ concatMap @B @[] . applyFun @A
    , testProperty "mfix" mkMfix

      -- Index
    , testProperty "adjust" $ adjust . applyFun @A
    , testProperty "update" $ update @A
    , testProperty "insertAt" $ insertAt @A
    , testProperty "deleteAt" $ deleteAt @A

      -- Slice
    , testProperty "cons" $ cons @A
    , testProperty "snoc" $ snoc @A
    , testProperty "uncons" $ fmap snd . uncons @A
    , testProperty "unsnoc" $ fmap fst . unsnoc @A
    , testProperty "take" $ take @A
    , testProperty "drop" $ drop @A
    , testProperty "slice" $ slice @A
    , testProperty "splitAt" $ splitAt @A
    , testProperty "takeEnd" $ takeEnd @A
    , testProperty "dropEnd" $ dropEnd @A
    , testProperty "splitAtEnd" $ splitAtEnd @A

      -- Transform
    , testProperty "fmap" $ map . applyFun @A @B
    , testProperty "liftA2" $ \(Sqrt1 xs) (Sqrt1 ys) (f :: Fun (A,B) C) ->
        liftA2 (applyFun2 f) xs ys
    , testProperty "traverse" $ runIdentity .: traverse . applyFun @A @(Identity B)
    , testProperty "imap" $ imap . applyFun2 @_ @A @B
    , testProperty "itraverse" $ runIdentity .: itraverse . applyFun2 @_ @A @(Identity B)
    , testProperty "reverse" $ reverse @A
    , testProperty "intersperse" $ intersperse @A
    , testProperty "scanl" $ scanl . applyFun2 @A @B
    , testProperty "scanr" $ scanr . applyFun2 @A @B
    , testProperty "sort" $ sort @A
    , testProperty "sortBy" $ sortBy @A compare

      -- Filter
    , testProperty "filter" $ filter . applyFun @A
    , testProperty "mapMaybe" $ mapMaybe . applyFun @A @(Maybe B)
    , testProperty "mapEither" $ mapEither . applyFun @A @(Either B C)
    , testProperty "takeWhile" $ takeWhile . applyFun @A
    , testProperty "dropWhile" $ dropWhile . applyFun @A
    , testProperty "span" $ span . applyFun @A
    , testProperty "break" $ break . applyFun @A
    , testProperty "takeWhileEnd" $ takeWhileEnd . applyFun @A
    , testProperty "dropWhileEnd" $ dropWhileEnd . applyFun @A
    , testProperty "spanEnd" $ spanEnd . applyFun @A
    , testProperty "breakEnd" $ breakEnd . applyFun @A

      -- Zip and unzip
    , testProperty "zipWith" $ zipWith . applyFun2 @A @B @C
    , testProperty "zipWith3" $ zipWith3 . applyFun3 @A @B @A @C
    , testProperty "unzipWith" $ unzipWith . applyFun @A @(B,C)
    , testProperty "unzipWith3" $ unzipWith3 . applyFun @A @(B,C,B)

      -- Random
    , testProperty "random transforms" $ \tf (xs :: MSeq A) ->
        let xs' = unTransform tf xs
        in classify (not (null xs')) "non-empty" xs'
    ]

  , testGroup "laws" $ L.map tastyLaws $
      let pa = Proxy @(MSeq A)
          porda = Proxy @(MSeq A)
      in
      [ QLaws.eqLaws pa
      , QLaws.ordLaws porda
      , QLaws.isListLaws pa
      , QLaws.semigroupLaws pa
      , QLaws.monoidLaws pa
      , QLaws.showLaws pa
      , QLaws.showReadLaws pa
      ]
  ]

instance (Arbitrary a, Measured a) => Arbitrary (MSeq a) where
  arbitrary = oneof
    [ fromList <$> arbitrary
    , fromRevList <$> arbitrary
    , randomStructure
    ]
    where
      randomStructure = sized $ \n -> do
        n' <- choose (0,n)
        if n' == 0
        then pure empty
        else Ap.liftA2 MSeqInternal.MTree arbitrary (go (n'-1))
        where
          go 0 = pure MTreeInternal.MTip
          go n = do
            ln <- choose (0,n-1)
            Ap.liftA3 MTreeInternal.link arbitrary (go ln) (go (n-1-ln))

  shrink = fmap fromList . shrink . F.toList

newtype Transform a = Transform { unTransform :: MSeq a -> MSeq a }

instance Show (Transform a) where
  show _ = "Transform"

instance (Measured a, Arbitrary a, CoArbitrary a) => Arbitrary (Transform a) where
  arbitrary = Transform . foldr (.) id <$> listOf tf
    where
      tf = oneof
        [ (fst .) . splitAt <$> arbitrary
        , (snd .) . splitAt <$> arbitrary
        , (<>) <$> arbitrary
        , flip (<>) <$> arbitrary
        , insertAt <$> arbitrary <*> arbitrary
        , deleteAt <$> arbitrary
        , map <$> arbitrary
        , fmap cons arbitrary
        , fmap (flip snoc) arbitrary
        , pure (maybe empty snd . uncons)
        , pure (maybe empty fst . unsnoc)
        , filter <$> arbitrary
        , mapMaybe <$> arbitrary
        ]

instance (Show a, Measured a, Eq (Measure a), Show (Measure a)) =>
  Testable (MSeq a) where
  property t =
    counterexample ("Invalid: " ++ MSeqInternal.debugShowsPrec 0 t "") $
      MSeqInternal.valid t

instance (Testable a, Testable b, a ~ MSeq x, b ~ MSeq y) => Testable (a, b) where
  property (a, b) = property a .&&. property b

instance
  (Testable a, Testable b, Testable c, a ~ MSeq x, b ~ MSeq y, c ~ MSeq z) =>
    Testable (a, b, c) where
  property (a, b, c) = property a .&&. property b .&&. property c

instance (Testable a, a ~ MSeq x) => Testable [a] where
  property = conjoin

instance Measured a => ListLike (MSeq a) where
  type E (MSeq a) = a
  fromL = fromList
  toL = F.toList

-- A non-commutative semigroup
-- From https://math.stackexchange.com/a/2893712
newtype Bato = Bato Integer deriving (Eq, Show)

instance Semigroup Bato where
  Bato x <> Bato y = Bato (abs x * y)

newtype A = A Integer
  deriving newtype (Eq, Ord, Read, Show, Arbitrary, CoArbitrary)

instance Function A where
  function = functionMap (\(A x) -> x) A

instance Measured A where
  type Measure A = Bato
  measure (A x) = Bato x

newtype B = B Integer
  deriving newtype (Eq, Ord, Show, Arbitrary, CoArbitrary)

instance Function B where
  function = functionMap (\(B x) -> x) B

instance Measured B where
  type Measure B = Bato
  measure (B x) = Bato x

newtype C = C Integer
  deriving newtype (Eq, Ord, Show, Arbitrary, CoArbitrary)

instance Function C where
  function = functionMap (\(C x) -> x) C

instance Measured C where
  type Measure C = Bato
  measure (C x) = Bato x

newtype D = D Integer
  deriving newtype (Eq, Ord, Show, Arbitrary, CoArbitrary)

instance Measured D where
  type Measure D = Maybe Bato
  measure (D x) = Just (Bato x)

data LI = LI [Integer] deriving (Eq, Show)

takeL :: Int -> LI -> LI
takeL n (LI xs) = LI (L.take n xs)

instance Measured LI where
  type Measure LI = Bato
  measure (LI xs) = Bato (head xs)

-- map (replicate 10) [0..n-1]
mkMfix :: Int -> MSeq LI
mkMfix n =
  map (takeL 10)
      (mfix (\ ~(LI is) -> generate n (\i -> LI (fromIntegral i : is))))

newtype SumElem = SumElem Word
  deriving newtype (Eq, Ord, Show, Arbitrary)

instance Measured SumElem where
  type Measure SumElem = Sum Word
  measure (SumElem x) = Sum x

-- Good enough for testing purposes
newtype Multiset a = Multiset [a]
  deriving newtype (Eq, Ord, Show, Semigroup)

countLessThanMultiset :: Ord a => a -> Multiset a -> Int
countLessThanMultiset k (Multiset xs) = length (L.filter (<k) xs)

newtype MultisetElem a = MultisetElem { unMultisetElem :: a }
  deriving newtype (Eq, Ord, Show, Arbitrary)

instance Measured (MultisetElem a) where
  type Measure (MultisetElem a) = Multiset a
  measure (MultisetElem x) = Multiset [x]
