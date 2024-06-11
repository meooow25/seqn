-- |
-- This is an internal module. You probably don't need to import this. Use
-- "Data.Seqn.Seq", "Data.Seqn.MSeq", or "Data.Seqn.PQueue" instead.
--
-- The only reason to use this module is to use the constructs defined here with
-- other internal modules.
--
module Data.Seqn.Internal.Util
  ( Biapplicative(..)
  , S2(..)
  , S3(..)
  , SMaybe(..)
  , Tagged(..)
  , SStateT(..)
  , evalSStateT
  , SState
  , sState
  , evalSState
  , (#.)
  ) where

import qualified Control.Applicative -- for before liftA2 in Prelude
import Data.Bifunctor (Bifunctor(..))
import Data.Coerce (Coercible, coerce)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))

class Bifunctor p => Biapplicative p where
  bipure :: a -> b -> p a b
  biliftA2 :: (a -> b -> c) -> (d -> e -> f) -> p a d -> p b e -> p c f

instance Biapplicative Const where
  bipure x _ = coerce x
  biliftA2 f _ = coerce f

data S2 a b = S2 !a !b

instance Functor (S2 a) where
  fmap f (S2 x y) = S2 x (f y)

instance Bifunctor S2 where
  bimap f g (S2 x y) = S2 (f x) (g y)

instance Biapplicative S2 where
  bipure = S2
  biliftA2 f g (S2 x1 y1) (S2 x2 y2) = S2 (f x1 x2) (g y1 y2)

data S3 a b c = S3 !a !b !c

data SMaybe a
  = SNothing
  | SJust !a

newtype Tagged a b = Tagged { unTagged :: b }

instance Functor (Tagged a) where
  fmap = coerce

instance Bifunctor Tagged where
  bimap _ = coerce

instance Biapplicative Tagged where
  bipure _ = coerce
  biliftA2 _ = coerce

-- Strict in the state, value, and bind.
newtype SStateT s m a = SStateT { runSStateT :: s -> m (S2 s a) }

evalSStateT :: Functor m => SStateT s m a -> s -> m a
evalSStateT m s = fmap (\(S2 _ x) -> x) (runSStateT m s)
{-# INLINE evalSStateT #-}

type SState s = SStateT s Identity

sState :: (s -> S2 s a) -> SState s a
sState = coerce

evalSState :: SState s a -> s -> a
evalSState m s = case runSStateT m s of Identity (S2 _ x) -> x
{-# INLINE evalSState #-}

instance Functor m => Functor (SStateT s m) where
  fmap f m = SStateT $ \s -> (fmap . fmap) f (runSStateT m s)
  {-# INLINE fmap #-}

instance Monad m => Applicative (SStateT s m) where
  pure x = SStateT $ \s -> pure $ S2 s x
  {-# INLINE pure #-}

  liftA2 f m1 m2 = SStateT $ \s -> do
    S2 s1 x <- runSStateT m1 s
    S2 s2 y <- runSStateT m2 s1
    pure $ S2 s2 (f x y)
  {-# INLINE liftA2 #-}

-- Borrow a trick from base in case GHC has trouble optimizing certain
-- function coercions.
--
-- See Note [Function coercion] in
-- https://gitlab.haskell.org/ghc/ghc/-/blob/8d67f247c3e4ca3810712654e1becbf927405f6b/libraries/ghc-internal/src/GHC/Internal/Data/Functor/Utils.hs#L134-160
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce
{-# INLINE (#.) #-}

infixr 9 #.
