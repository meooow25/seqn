{-# OPTIONS_GHC -Wno-orphans #-} -- Arbitrary instances
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Seq (seqTests) where

import Prelude hiding (unzip3, zip3, unzip, zip, break, concatMap, drop, dropWhile, filter, lookup, replicate, reverse, splitAt, scanl, scanr, span, take, takeWhile, zipWith, zipWith3)
import qualified Control.Applicative as Ap
import Data.Coerce (coerce)
import Control.Monad.Fix (MonadFix(..))
import qualified Data.Foldable as F
import Data.Foldable (toList)
import qualified Data.Foldable.WithIndex as IFo
import Data.Functor.Identity (Identity(..))
import qualified Data.Functor.WithIndex as IFu
import qualified Data.List as L
import Data.Proxy (Proxy(..))
import Data.Semigroup (stimes)
import Data.Tuple (Solo(..))
import qualified Data.Traversable.WithIndex as ITr
import Test.Tasty
import Test.Tasty.QuickCheck hiding (generate)
import qualified Test.QuickCheck.Classes.Base as QLaws
import Test.QuickCheck.Poly (A, B, C, OrdA)

import Data.Seqn.Seq
import qualified Data.Seqn.Internal.Seq as SeqInternal
import qualified Data.Seqn.Internal.Tree as TreeInternal
import ListExtra (unsnocL)
import qualified ListLikeTests as LL
import TestUtil (Sqrt1(..), tastyLaws, (.:), (.:.), ListLike(..))

seqTests :: TestTree
seqTests = testGroup "Data.Seqn.Seq"
  [ testGroup "properties"
    [
      LL.listLike @(Seq A)

      -- Construct
    , LL.empty @(Seq A) empty
    , LL.singleton @(Seq A) singleton
    , LL.replicate @(Seq A) replicate
    , LL.replicateA @(Seq A) replicateA
    , LL.generate @(Seq A) generate
    , LL.generateA @(Seq A) generateA
    , LL.unfoldr @(Seq A) unfoldr
    , LL.unfoldl @(Seq A) unfoldl
    , LL.unfoldrM @(Seq A) unfoldrM
    , LL.unfoldlM @(Seq A) unfoldlM
    , (LL.<>) @(Seq A) (<>)
    , LL.stimes @(Seq A) stimes
    , LL.mconcat @(Seq A) mconcat
    , LL.concatMap @(Seq A) concatMap
    , testProperty "mfix" $ \n ->
        toList (mkMfix n) === fmap (MkSolo . L.replicate 10) [0 .. n-1]
    , LL.read @(Seq Int)

      -- Convert
    , LL.toRevList @(Seq A) toRevList
    , LL.show @(Seq A)

      -- Fold
    , LL.foldMap @(Seq A) foldMap
    , LL.foldMap' @(Seq A) F.foldMap'
    , LL.foldr @(Seq A) foldr
    , LL.foldl @(Seq A) F.foldl
    , LL.foldl' @(Seq A) F.foldl'
    , LL.foldr' @(Seq A) F.foldr'
    , LL.ifoldMap @(Seq A) IFo.ifoldMap
    , LL.ifoldMap' @(Seq A) IFo.ifoldMap'
    , LL.ifoldr @(Seq A) IFo.ifoldr
    , LL.ifoldl @(Seq A) IFo.ifoldl
    , LL.ifoldr' @(Seq A) IFo.ifoldr'
    , LL.ifoldl' @(Seq A) IFo.ifoldl'

      -- Index
    , LL.lookup @(Seq A) lookup
    , LL.index @(Seq A) index
    , LL.update @(Seq A) update
    , LL.adjust @(Seq A) adjust
    , LL.insertAt @(Seq A) insertAt
    , LL.deleteAt @(Seq A) deleteAt

      -- Slice
    , LL.cons @(Seq A) cons
    , LL.snoc @(Seq A) snoc
    , LL.uncons @(Seq A) uncons
    , LL.unsnoc @(Seq A) unsnoc
    , LL.take @(Seq A) take
    , LL.drop @(Seq A) drop
    , LL.slice @(Seq A) slice
    , LL.splitAt @(Seq A) splitAt
    , LL.takeEnd @(Seq A) takeEnd
    , LL.dropEnd @(Seq A) dropEnd
    , LL.splitAtEnd @(Seq A) splitAtEnd
    , LL.tails @(Seq A) tails
    , LL.inits @(Seq A) inits
    , LL.chunksOf @(Seq A) chunksOf

      -- Filter
    , LL.filter @(Seq A) filter
    , LL.mapMaybe @(Seq A) @(Seq B) mapMaybe
    , LL.mapEither @(Seq A) @(Seq B) @(Seq C) mapEither
    , LL.filterM @(Seq A) filterA
    , LL.mapMaybeM @(Seq A) @(Seq B) mapMaybeA
    , LL.mapEitherM @(Seq A) @(Seq B) @(Seq C) mapEitherA
    , LL.takeWhile @(Seq A) takeWhile
    , LL.dropWhile @(Seq A) dropWhile
    , LL.span @(Seq A) span
    , LL.break @(Seq A) break
    , LL.takeWhileEnd @(Seq A) takeWhileEnd
    , LL.dropWhileEnd @(Seq A) dropWhileEnd
    , LL.spanEnd @(Seq A) spanEnd
    , LL.breakEnd @(Seq A) breakEnd

      -- Transform
    , LL.map @(Seq A) @(Seq B) fmap
    , LL.liftA2 @(Sqrt1 Seq A) @(Sqrt1 Seq B) @(Sqrt1 Seq C) (coerce (Ap.liftA2 @Seq @A @B @C))
    , (LL.<*) @(Seq A) @(Seq B) (<*)
    , (LL.*>) @(Seq A) @(Seq B) (*>)
    , LL.bind @(Seq A) @(Seq B) (>>=)
    , LL.traverse @(Seq A) @(Seq B) traverse
    , LL.imap @(Seq A) @(Seq B) IFu.imap
    , LL.itraverse @(Seq A) @(Seq B) ITr.itraverse
    , LL.reverse @(Seq A) reverse
    , LL.intersperse @(Seq A) intersperse
    , LL.scanl @(Seq A) @(Seq B) scanl
    , LL.scanr @(Seq A) @(Seq B) scanr
    , LL.sort @(Seq OrdA) sort
    , LL.sortBy @(Seq A) sortBy

      -- Search and test
    , LL.eq @(Seq A) (==)
    , LL.cmp @(Seq OrdA) compare
    , LL.findEnd @(Seq A) findEnd
    , LL.findIndex @(Seq A) findIndex
    , LL.findIndexEnd @(Seq A) findIndexEnd
    , LL.infixIndices @(Seq A) infixIndices
    , LL.binarySearchFind @(Seq OrdA) binarySearchFind
    , LL.isPrefixOf @(Seq A) isPrefixOf
    , LL.isSuffixOf @(Seq A) isSuffixOf
    , LL.isInfixOf @(Seq A) isInfixOf
    , LL.isSubsequenceOf @(Seq A) isSubsequenceOf

      -- Zip and unzip
    , LL.zip @(Seq A) @(Seq B) zip
    , LL.zip3 @(Seq A) @(Seq B) @(Seq C) zip3
    , LL.zipWith @(Seq A) @(Seq B) @(Seq C) zipWith
    , LL.zipWith3 @(Seq A) @(Seq B) @(Seq C) @(Seq A) zipWith3
    , LL.zipWithM @(Seq A) @(Seq B) @(Seq C) zipWithM
    , LL.zipWith3M @(Seq A) @(Seq B) @(Seq C) @(Seq A) zipWith3M
    , LL.unzip @(Seq (A, B)) unzip
    , LL.unzip3 @(Seq (A, B, C)) unzip3
    , LL.unzipWith @(Seq A) @(Seq B) @(Seq C) unzipWith
    , LL.unzipWith3 @(Seq A) @(Seq B) @(Seq C) @(Seq A) unzipWith3
    ]

  , testGroup "valid"
    [
      -- Arbitrary
      testProperty "arbitrary" $ id @(Seq A)

      -- Construct
    , testProperty "fromList" $ fromList @A
    , testProperty "fromRevList" $ fromRevList @A
    , testProperty "replicate" $ replicate @A
    , testProperty "generate" $ \n -> generate n . applyFun @Int @A
    , testProperty "unfoldr" $ unfoldr (L.uncons @A)
    , testProperty "unfoldl" $ unfoldl (unsnocL @A)
    , testProperty "<>" $ (<>) @(Seq A)
    , testProperty "stimes" $ stimes @(Seq A) @Int
    , testProperty "mconcat" $ mconcat @(Seq A)
    , testProperty "concatMap" $ concatMap @[] . applyFun @A @(Seq B)
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
    , testProperty "tails" $ tails @A
    , testProperty "tails each" $ toList . tails @A
    , testProperty "inits" $ inits @A
    , testProperty "inits each" $ toList . inits @A
    , testProperty "chunksOf" $ chunksOf @A
    , testProperty "chunksOf each" $ toList .: chunksOf @A

      -- Transform
    , testProperty "fmap" $ fmap @Seq . applyFun @A @B
    , testProperty "liftA2" $ unSqrt1 .:. Ap.liftA2 @(Sqrt1 Seq) . applyFun2 @A @B @C
    , testProperty "<*" $ (<*) @Seq @A @B
    , testProperty "*>" $ (*>) @Seq @A @B
    , testProperty ">>=" $ flip ((>>=) @Seq) . applyFun @A @(Seq B)
    , testProperty "traverse" $ runIdentity .: traverse @Seq . applyFun @A @(Identity B)
    , testProperty "imap" $ IFu.imap @_ @Seq . applyFun2 @_ @A @B
    , testProperty "itraverse" $ runIdentity .: ITr.itraverse @_ @Seq . applyFun2 @_ @A @(Identity B)
    , testProperty "reverse" $ reverse @A
    , testProperty "intersperse" $ intersperse @A
    , testProperty "scanl" $ scanl . applyFun2 @A @B
    , testProperty "scanr" $ scanr . applyFun2 @A @B
    , testProperty "sort" $ sort @OrdA
    , testProperty "sortBy" $ sortBy @OrdA compare

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

      -- Zip
    , testProperty "zipWith" $ zipWith . applyFun2 @A @B @C
    , testProperty "zipWith3" $ zipWith3 . applyFun3 @A @B @A @C
    , testProperty "unzipWith" $ unzipWith . applyFun @A @(B,C)
    , testProperty "unzipWith3" $ unzipWith3 . applyFun @A @(B,C,B)

      -- Random
    , testProperty "random transforms" $ \tf (xs :: Seq A) ->
        let xs' = unTransform tf xs
        in classify (not (null xs')) "non-empty" xs'
    ]

  , testGroup "laws" $ map tastyLaws $
    let p = Proxy @Seq
        sqrtp = Proxy @(Sqrt1 Seq)
        pa = Proxy @(Seq A)
        porda = Proxy @(Seq OrdA)
        pint = Proxy @(Seq Int)
    in
    [ QLaws.eqLaws pa
    , QLaws.ordLaws porda
    , QLaws.isListLaws pa
    , QLaws.semigroupLaws pa
    , QLaws.monoidLaws pa
    , QLaws.showLaws pa
    , QLaws.showReadLaws pint
    , QLaws.foldableLaws p
    , QLaws.traversableLaws p
    , QLaws.functorLaws p
    , QLaws.applicativeLaws sqrtp
    , QLaws.alternativeLaws p
    , QLaws.monadLaws sqrtp
    , QLaws.monadPlusLaws p
    , QLaws.monadZipLaws p
    ]
  ]

instance Arbitrary a => Arbitrary (Seq a) where
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
        else Ap.liftA2 SeqInternal.Tree arbitrary (go (n'-1))
        where
          go 0 = pure TreeInternal.Tip
          go n = do
            ln <- choose (0,n-1)
            Ap.liftA3 TreeInternal.link arbitrary (go ln) (go (n-1-ln))

  shrink = fmap fromList . shrink . F.toList

newtype Transform a = Transform { unTransform :: Seq a -> Seq a }

instance Show (Transform a) where
  show _ = "Transform"

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Transform a) where
  arbitrary = Transform . foldr (.) id <$> listOf tf
    where
      tf = oneof
        [ (fst .) . splitAt <$> arbitrary
        , (snd .) . splitAt <$> arbitrary
        , (<>) <$> arbitrary
        , flip (<>) <$> arbitrary
        , insertAt <$> arbitrary <*> arbitrary
        , deleteAt <$> arbitrary
        , fmap <$> arbitrary
        , fmap cons arbitrary
        , fmap (flip snoc) arbitrary
        , pure (maybe empty snd . uncons)
        , pure (maybe empty fst . unsnoc)
        , filter <$> arbitrary
        , mapMaybe <$> arbitrary
        ]

instance Show a => Testable (Seq a) where
  property t =
    counterexample ("Invalid: " ++ SeqInternal.debugShowsPrec 0 t "") $
      SeqInternal.valid t

instance (Testable a, Testable b, a ~ Seq x, b ~ Seq y) => Testable (a, b) where
  property (a, b) = property a .&&. property b

instance
  (Testable a, Testable b, Testable c, a ~ Seq x, b ~ Seq y, c ~ Seq z) =>
    Testable (a, b, c) where
  property (a, b, c) = property a .&&. property b .&&. property c

instance (Testable a, a ~ Seq x) => Testable [a] where
  property = conjoin

instance ListLike (Seq a) where
  type E (Seq a) = a
  fromL = fromList
  toL = toList

-- map (replicate 10) [0..n-1]
mkMfix :: Int -> Seq (Solo [Int])
mkMfix n =
  fmap (fmap (L.take 10))
       (mfix (\ ~(MkSolo is) -> generate n (\i -> MkSolo (i : is))))
