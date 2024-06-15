-- |
-- = Priority queues
--
-- @PQueue@ is a minimum priority queue implemented using an
-- t'Data.Seqn.MSeq.MSeq'.
--
-- * It is spine-strict, and can contain only a finite number of elements.
-- * It is value-strict. It is guaranteed that if a @PQueue@ is in
--   [weak head normal form](https://wiki.haskell.org/Weak_head_normal_form)
--   (WHNF), every element of the @PQueue@ is also in WHNF.
-- * It maintains insertion order. If two elements compare equal, the one
--   which was inserted first will be removed first. Elements can also be
--   folded over in insertion order.
-- * It is a mergeable priority queue. Two queues can be concatenated
--   efficiently in logarithmic time.
--
-- It is recommended to import this module qualified to avoid name clashes.
--
-- @
-- import Data.Seqn.PQueue (PQueue)
-- import qualified Data.Seqn.PQueue as PQueue
-- @
--
module Data.Seqn.PQueue
  (
    -- * PQueue
    P.PQueue
  , P.empty
  , P.singleton
  , P.fromList
  , P.concatMap
  , P.insert
  , P.min
  , P.minView
  , P.toSortedList

    -- * Entry
  , P.Entry(..)
  , P.entryPrio
  , P.entryValue
  ) where

import qualified Data.Seqn.Internal.PQueue as P
