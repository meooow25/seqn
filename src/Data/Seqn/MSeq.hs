-- |
-- = Finite measured sequences
--
-- A value of type @MSeq a@ is a sequence with elements of type @a@.
-- An @MSeq@ is
--
-- * Spine-strict, hence finite. @MSeq@ cannot represent infinite sequences.
-- * Value-strict. It is guaranteed that if an @MSeq@ is in
--   [weak head normal form](https://wiki.haskell.org/Weak_head_normal_form)
--   (WHNF), every element of the @Seq@ is also in WHNF.
--
-- An @MSeq@ provides quick access to the combined \"measure\" of all elements
-- of the sequence. Please see the [Tutorial](#g:tutorial) at the end of this
-- page for an explanation.
--
-- It is recommended to import this module qualified to avoid name clashes.
--
-- @
-- import Data.Seqn.MSeq (Measured, MSeq)
-- import qualified Data.Seqn.MSeq as MSeq
-- @
--
-- === Warning
--
-- The length of an @MSeq@ must not exceed @(maxBound \`div\` 3) :: Int@. If
-- this length is exceeded, the behavior of an @MSeq@ is undefined. This value
-- is very large in practice, greater than \(7 \cdot 10^8\) on 32-bit systems
-- and \(3 \cdot 10^{18}\) on 64-bit systems.
--
-- === Note on time complexities
--
-- Many functions operating on @MSeq a@ require a @Measured a@ constraint. The
-- documented time complexities of these functions assume that
-- @measure :: a -> Measure a@ and
-- @(<>) :: Measure a -> Measure a -> Measure a@ both take \(O(1)\) time. If
-- this not the case, the bounds do not hold. Correct bounds can be calculated
-- by the user depending on their implementations of @measure@ and @(<>)@.
--
-- === Implementation
--
-- @MSeq@ is implemented as a
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
module Data.Seqn.MSeq
  (
    -- * MSeq
    S.MSeq
  , M.Measured(..)

    -- * Measured queries
  , S.summaryMay
  , S.summary
  , S.sliceSummaryMay
  , S.sliceSummary
  , S.foldlSliceSummaryComponents
  , S.binarySearchPrefix
  , S.binarySearchSuffix

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
  , S.mfix

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

    -- * Filter
  , S.filter
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
  , S.map
  , S.liftA2
  , S.traverse
  , S.imap
  , S.itraverse
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
  , S.zipWith
  , S.zipWith3
  , S.zipWithM
  , S.zipWith3M
  , S.unzipWith
  , S.unzipWith3

    -- * Force
  , S.liftRnf2

    -- * Tutorial #tutorial#
    -- $tutorial
  ) where

import qualified Data.Seqn.Internal.MTree as M
import qualified Data.Seqn.Internal.MSeq as S

-- $tutorial
--
-- @MSeq@, like @Seq@, is a sequence which supports operations like @lookup@,
-- @splitAt@, @(<>)@, @foldr@, and more.
--
-- Additionally, every element in an @MSeq@ has an associated \"measure\".
-- Such measures can be combined using a @Semigroup@ instance. An @MSeq@ allows
-- accessing the combined measure of all its elements in \(O(1)\) time. The
-- choice of the measure depends on the use case.
--
-- == Example 1: Sum
--
-- @
-- data Task = Task
--   !Text -- ^ Name
--   !Word -- ^ Cost
--   deriving Show
-- @
--
-- Consider that we need to maintain a sequence of tasks, where each task has
-- some cost. Tasks will be added and removed over time. At various points, the
-- total cost of all the tasks in the sequence must be computed.
--
-- We may use a @Seq@ to store the task, and calculate the sum when required in
-- \(O(n)\). This is reasonable if such events are rare, but a poor strategy
-- if the sum has to be calculated frequently. In the latter case, we could use
-- an @MSeq@.
--
-- First, some imports.
--
-- @
-- import Data.Seqn.MSeq (Measured, MSeq)
-- import qualified Data.Seqn.MSeq as MSeq
-- @
--
-- Next, we define the t'Data.Seqn.MSeq.Measured' instance for @Task@.
--
-- @
-- {-# LANGUAGE TypeFamilies #-}
-- import "Data.Monoid" (Sum(..))
--
-- instance Measured Task where
--   type Measure Task = Sum Word
--   measure (Task _ cost) = Sum cost
-- @
--
-- >>> let tasks = MSeq.fromList [Task "A" 50, Task "B" 30, Task "C" 60]
-- >>> tasks
-- [Task "A" 50,Task "B" 30,Task "C" 60]
--
-- We now have access to the combined measure of the @MSeq@, called the
-- 'Data.Seqn.MSeq.summary', in \(O(1)\).
--
-- >>> MSeq.summary tasks
-- Sum {getSum = 140}
--
-- If we modify the task list, the summary will change accordingly.
--
-- >>> let tasks' = MSeq.deleteAt 2 $ MSeq.cons (Task "D" 100) tasks
-- >>> tasks'
-- [Task "D" 100,Task "A" 50,Task "C" 60]
-- >>> MSeq.summary tasks'
-- Sum {getSum = 210}
--
-- == Example 2: Max
--
-- Consider that we now need the maximum cost instead of the sum, or both
-- sum and max. We need only change the @Measured@ instance to use another
-- @Semigroup@ that fits the requirement.
--
-- @
-- data SumMax = SumMax
--   { sum_ :: !Word
--   , max_ :: !Word
--   } deriving Show
--
-- instance Semigroup SumMax where
--   SumMax sum1 max1 <> SumMax sum2 max2 =
--     SumMax (sum1+sum2) (max max1 max2)
--
-- instance Measured Task where
--   type Measure Task = SumMax
--   measure (Task _ cost) = SumMax cost cost
-- @
--
-- We can see that it works as expected.
--
-- >>> let tasks = MSeq.fromList [Task "A" 50, Task "B" 30, Task "C" 60]
-- >>> MSeq.summaryMay tasks
-- Just (SumMax {sum_ = 140, max_ = 60})
--
-- Note that we used 'Data.Seqn.MSeq.summaryMay' instead of @summary@, since we
-- did not define a monoid instance for @SumMax@.
--
-- Aside: For the above scenario you may have considered using @(Sum Word,
-- t'Data.Monoid.Max' Word)@ as the measure, since the @Semigroup@ instance for
-- it is already defined. While that would work, it would be inefficient because
-- @(a,b)@ and its @(<>)@ implementation are lazy in @a@ and @b@.
--
-- == Example 3: Binary search
--
-- Consider that there are events where we unlock the ability to process tasks
-- with a total cost \(c\). To handle such events, we need to split out the
-- maximum number of tasks from the beginning of the sequence such that their
-- total does not exceed \(c\), and send them for processing.
--
-- We can do this efficiently with an @MSeq@. The prefix sums of costs, which
-- is a component of our measure, forms a monotonic non-decreasing sequence.
-- We can take advantage of this and use binary search to find the point where
-- the sequence should be split.
--
-- @
-- splitAtMost :: Word -> MSeq Task -> Maybe (MSeq Task, MSeq Task)
-- splitAtMost c tasks =
--   case MSeq.'Data.Seqn.MSeq.binarySearchPrefix' (\\(SumMax c' _) -> c' > c) tasks of
--     (Nothing, _) -> Nothing -- c is too small for even the first task
--     (Just i, _) -> Just $! MSeq.'Data.Seqn.MSeq.splitAt' (i+1) tasks
-- @
--
-- >>> let tasks = MSeq.fromList [Task "A" 50, Task "B" 30, Task "C" 60]
-- >>> splitAtMost 100 tasks
-- Just ([Task "A" 50,Task "B" 30],[Task "C" 60])
-- >>> splitAtMost 10 tasks
-- Nothing
--
-- The running time of @splitAtMost@ is simply \(O(\log n)\), and it does not
-- depend on how many tasks are split out.
--
-- == More uses
--
-- More uses of measured sequences can be found in the paper on finger trees:
--
-- * Ralf Hinze and Ross Paterson,
--   /\"Finger trees: a simple general-purpose data structure\"/,
--   Journal of Functional Programming 16(2), 197-217, 2006,
--   https://doi.org/10.1017/S0956796805005769
--
-- One such use, priority queues, is implemented in this package and can be
-- found in the module "Data.Seqn.PQueue".
