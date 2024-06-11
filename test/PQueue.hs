{-# OPTIONS_GHC -Wno-orphans #-} -- Arbitrary instances
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module PQueue (pQueueTests) where

import Prelude hiding (concatMap, min)
import qualified Data.Foldable as F
import Data.Foldable (toList)
import qualified Data.Foldable.WithIndex as IFo
import qualified Data.List as L
import Data.Proxy (Proxy(..))
import qualified Test.QuickCheck.Classes.Base as QLaws
import Test.QuickCheck.Poly (A, OrdA)
import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Seqn.PQueue
import TestUtil (ListLike(..), tastyLaws)
import qualified ListLikeTests as LL

pQueueTests :: TestTree
pQueueTests = testGroup "Data.Seqn.PQueue"
  [ testGroup "properties"
    [
      LL.listLike @(PQueue OrdA)

    , LL.empty @(PQueue OrdA) empty
    , LL.singleton @(PQueue OrdA) singleton
    , LL.snoc @(PQueue OrdA) (flip insert)
    , LL.concatMap @(PQueue OrdA) concatMap
    , LL.read @(PQueue Int)

    , testProperty "min" $ \(q :: PQueue EA) ->
        min q === minL (toList q)
    , testProperty "minView" $ \(q :: PQueue EA) ->
        (fmap . fmap) toList (minView q) === minViewL (toList q)
    , testProperty "toSortedList" $ \(q :: PQueue EA) ->
        toSortedList q === L.sort (toList q)

    , LL.foldMap @(PQueue OrdA) foldMap
    , LL.foldMap' @(PQueue OrdA) F.foldMap'
    , LL.foldr @(PQueue OrdA) foldr
    , LL.foldl' @(PQueue OrdA) F.foldl'
    , LL.foldl @(PQueue OrdA) foldl
    , LL.foldr' @(PQueue OrdA) F.foldr'
    , LL.ifoldMap @(PQueue OrdA) IFo.ifoldMap
    , LL.ifoldMap' @(PQueue OrdA) IFo.ifoldMap'
    , LL.ifoldr @(PQueue OrdA) IFo.ifoldr
    , LL.ifoldr' @(PQueue OrdA) IFo.ifoldr'
    , LL.ifoldl @(PQueue OrdA) IFo.ifoldl
    , LL.ifoldl' @(PQueue OrdA) IFo.ifoldl'
    , LL.show @(PQueue OrdA)
    ]

  , testGroup "laws" $ map tastyLaws $
    let pe = Proxy @(PQueue EA)
        pint = Proxy @(PQueue Int)
    in
    [ QLaws.eqLaws pe
    , QLaws.ordLaws pe
    , QLaws.isListLaws pe
    , QLaws.semigroupLaws pe
    , QLaws.monoidLaws pe
    , QLaws.showLaws pe
    , QLaws.showReadLaws pint
    ]
  ]

-- Use Entry instead of just OrdA to test queue stability.
type EA = Entry OrdA A

instance (Arbitrary k, Arbitrary a) => Arbitrary (Entry k a) where
  arbitrary = liftA2 Entry arbitrary arbitrary
  shrink (Entry k x) = uncurry Entry <$> shrink (k,x)

instance (Ord a, Arbitrary a) => Arbitrary (PQueue a) where
  arbitrary = fromList <$> arbitrary
  shrink = fmap fromList . shrink . toList

instance (CoArbitrary k, CoArbitrary a) => CoArbitrary (Entry k a) where
  coarbitrary (Entry k x) = coarbitrary (k,x)

instance (Function k, Function a) => Function (Entry k a) where
  function = functionMap (\(Entry k x) -> (k,x)) (uncurry Entry)

instance Ord a => ListLike (PQueue a) where
  type E (PQueue a) = a
  fromL = fromList
  toL = F.toList

minL :: Ord a => [a] -> Maybe a
minL [] = Nothing
minL xs = Just (minimum xs)

minViewL :: Ord a => [a] -> Maybe (a, [a])
minViewL xs = fmap (\x -> (x, xs L.\\ [x])) (minL xs)
