-- |
-- = Finite sequences
--
-- A value of type @Seq a@ is a sequence with elements of type @a@.
-- A @Seq@ is
--
-- * Spine-strict, hence finite. @Seq@ cannot represent infinite sequences.
-- * Value-strict. It is guaranteed that if a @Seq@ is in
--   [weak head normal form](https://wiki.haskell.org/Weak_head_normal_form)
--   (WHNF), every element of the @Seq@ is also in WHNF.
--
-- It is recommended to import this module qualified to avoid name clashes.
--
-- @
-- import Data.Seqn.Seq (Seq)
-- import qualified Data.Seqn.Seq as Seq
-- @
--
-- === Warning
--
-- The length of a @Seq@ must not exceed @(maxBound \`div\` 3) :: Int@. If this
-- length is exceeded, the behavior of a @Seq@ is undefined. This value is very
-- large in practice, greater than \(7 \cdot 10^8\) on 32-bit systems and
-- \(3 \cdot 10^{18}\) on 64-bit systems.
--
-- === Implementation
--
-- @Seq@ is implemented as a
-- [weight-balanced binary tree](https://en.wikipedia.org/wiki/Weight-balanced_tree).
-- This structure is described by
--
-- * J. Nievergelt and E. M. Reingold,
--   /\"Binary search trees of bounded balance\"/,
--   SIAM Journal of Computing 2(1), 1973,
--   https://doi.org/10.1137/0202005
--
-- * Stephen Adams,
--   /\"Efficient sets—a balancing act\"/,
--   Journal of Functional Programming 3(4), 553-561, 1993,
--   https://doi.org/10.1017/S0956796800000885
--
-- * Yoichi Hirai and Kazuhiko Yamamoto,
--   /\"Balancing weight-balanced trees\"/,
--   Journal of Functional Programming 21(3), 287-307, 2011,
--   https://doi.org/10.1017/S0956796811000104
--
-- * Guy Blelloch, Daniel Ferizovic, and Yihan Sun,
--   /\"Parallel Ordered Sets Using Join\"/, 2016,
--   https://doi.org/10.48550/arXiv.1602.02120
--
module Data.Seqn.Seq
  (
    -- * Seq
    S.Seq

    -- * Construct
  , S.empty
  , S.singleton
  , S.fromList
  , S.fromRevList
  , S.replicate
  , S.replicateA
  , S.generate
  , S.generateA
  , S.unfoldr
  , S.unfoldl
  , S.unfoldrM
  , S.unfoldlM
  , S.concatMap

    -- * Convert
  , S.toRevList

    -- * Index
  , S.lookup
  , S.index
  , (S.!?)
  , (S.!)
  , S.update
  , S.adjust
  , S.insertAt
  , S.deleteAt

    -- * Slice
  , S.cons
  , S.snoc
  , S.uncons
  , S.unsnoc
  , S.take
  , S.drop
  , S.slice
  , S.splitAt
  , S.takeEnd
  , S.dropEnd
  , S.splitAtEnd
  , S.tails
  , S.inits
  , S.chunksOf

    -- * Filter
  , S.filter
  , S.catMaybes
  , S.mapMaybe
  , S.mapEither
  , S.filterA
  , S.mapMaybeA
  , S.mapEitherA
  , S.takeWhile
  , S.dropWhile
  , S.span
  , S.break
  , S.takeWhileEnd
  , S.dropWhileEnd
  , S.spanEnd
  , S.breakEnd

    -- * Transform
  , S.reverse
  , S.intersperse
  , S.scanl
  , S.scanr
  , S.sort
  , S.sortBy

    -- * Search and test
  , S.findEnd
  , S.findIndex
  , S.findIndexEnd
  , S.infixIndices
  , S.binarySearchFind
  , S.isPrefixOf
  , S.isSuffixOf
  , S.isInfixOf
  , S.isSubsequenceOf

    -- * Zip and unzip
  , S.zip
  , S.zip3
  , S.zipWith
  , S.zipWith3
  , S.zipWithM
  , S.zipWith3M
  , S.unzip
  , S.unzip3
  , S.unzipWith
  , S.unzipWith3
  ) where

import qualified Data.Seqn.Internal.Seq as S
