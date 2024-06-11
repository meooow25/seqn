{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TestUtil
  ( ListLike(..)
  , Sqrt1(..)
  , tastyLaws
  , MaybeBottom(..)
  , isBottom
  , eval
  , (.:)
  , (.:.)
  ) where

import Control.Exception (try, evaluate, ErrorCall)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes.Base (Laws(..))

class ListLike t where
  type E t
  fromL :: [E t] -> t
  toL :: t -> [E t]

newtype Sqrt1 f a = Sqrt1 { unSqrt1 :: f a }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Functor, Applicative, Monad)

instance Arbitrary (f a) => Arbitrary (Sqrt1 f a) where
  arbitrary = sized $ \n -> Sqrt1 <$> resize (isqrt n) arbitrary
    where
      isqrt n = round (sqrt (fromIntegral n) :: Double)
  shrink = fmap Sqrt1 . shrink . unSqrt1

instance (ListLike (f a), E (f a) ~ a) => ListLike (Sqrt1 f a) where
  type E (Sqrt1 f a) = a
  fromL = Sqrt1 . fromL
  toL = toL . unSqrt1

data MaybeBottom a = Bottom | NotBottom a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (MaybeBottom a) where
  arbitrary = frequency [(1, pure Bottom), (5, NotBottom <$> arbitrary)]
  shrink = \case
    Bottom -> []
    NotBottom x -> Bottom : fmap NotBottom (shrink x)

isBottom :: MaybeBottom a -> Bool
isBottom = \case
  Bottom -> True
  NotBottom _ -> False

eval :: a -> IO (MaybeBottom a)
eval = fmap (either @ErrorCall (const Bottom) NotBottom) . try . evaluate

tastyLaws :: Laws -> TestTree
tastyLaws (Laws class_ tests) =
  testGroup class_ (map (uncurry testProperty) tests)

infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

infixr 8 .:.
(.:.) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:.) = (.) . (.:)
