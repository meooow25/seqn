{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import Control.DeepSeq (NFData(..), deepseq, rwhnf)
import Control.Monad (join)
import qualified Data.Foldable.WithIndex as IFo
import qualified Data.Foldable as F
import qualified Data.Functor.WithIndex as IFu
import Data.Bits ((.&.))
import qualified Data.List as L
import Data.Monoid (Sum(..))

import Test.Tasty.Bench

import qualified Data.FingerTree as FT
import qualified Data.PQueue.Prio.Min as PQPM
import qualified Data.PriorityQueue.FingerTree as FTPQ
import qualified Data.RRBVector as RRB
import qualified Data.Seqn.MSeq as MSeq
import qualified Data.Seqn.PQueue as PQ
import qualified Data.Seqn.Seq as Seq
import qualified Data.Sequence as FTSeq

main :: IO ()
main = defaultMain
  [ seqBenches
  , pqBenches
  ]

seqBenches :: Benchmark
seqBenches = envp seqData $ \_ -> bgroup "Sequence"
  [ bgroup "rnf"
    [ bench "Seq" $ nf id bigSeq
    , bench "MSeq" $ nf id bigMSeq
    , bench "Sequence" $ nf id bigFTSeq
    , bench "RRBVector" $ nf id bigRRB
    , bench "FingerTree" $ nfFoldable id bigFT
    ]
  , bgroup "fromList"
    [ bench "Seq" $ whnf Seq.fromList bigList
    , bench "Seq rev" $ whnf Seq.fromRevList bigList
    , bench "MSeq" $ whnf MSeq.fromList bigList
    , bench "Sequence" $ whnf FTSeq.fromList bigList
    , bench "RRBVector" $ whnf RRB.fromList bigList
    , bench "FingerTree" $ whnf FT.fromList bigList
    ]
  , bgroup "fromList nf"
    [ bench "Seq" $ nf Seq.fromList bigList
    , bench "Seq rev" $ nf Seq.fromRevList bigList
    , bench "MSeq" $ nf MSeq.fromList bigList
    , bench "Sequence" $ nf FTSeq.fromList bigList
    , bench "RRBVector" $ nf RRB.fromList bigList
    , bench "FingerTree" $ nfFoldable FT.fromList bigList
    ]
  , bgroup "fromList fusion"
    [ bench "Seq" $ whnf (Seq.fromList . (\x -> [1..x])) bigN
    , bench "Seq rev" $ whnf (Seq.fromRevList . (\x -> [1..x])) bigN
    , bench "MSeq" $ whnf (MSeq.fromList . (\x -> [1..x])) bigN
    , bench "Sequence" $ whnf (FTSeq.fromList . (\x -> [1..x])) bigN
    , bench "RRBVector" $ whnf (RRB.fromList . (\x -> [1..x])) bigN
    , bench "FingerTree" $ whnf (FT.fromList . (\x -> [1..x])) bigN
    ]
  , bgroup "foldr short-circuit" $
    let f x z = x == (bigN `div` 2) || z in
    [ bench "Seq" $ whnf (foldr f False) bigSeq
    , bench "MSeq" $ whnf (foldr f False) bigMSeq
    , bench "Sequence" $ whnf (foldr f False) bigFTSeq
    , bench "RRBVector" $ whnf (foldr f False) bigRRB
    , bench "FingerTree" $ whnf (foldr f False) bigFT
    ]
  , bgroup "cps sum foldr" $
    let f x k = \acc -> if acc < 0 then acc else k (x+acc) in
    [ bench "Seq" $ whnf (\xs -> foldr f id xs 0) bigSeq
    , bench "MSeq" $ whnf (\xs -> foldr f id xs 0) bigMSeq
    , bench "Sequence" $ whnf (\xs -> foldr f id xs 0) bigFTSeq
    , bench "RRBVector" $ whnf (\xs -> foldr f id xs 0) bigRRB
    , bench "FingerTree" $ whnf (\xs -> foldr f id xs 0) bigFT
    ]
  , bgroup "foldl short-circuit" $
    let f z x = x == (bigN `div` 2) || z in
    [ bench "Seq" $ whnf (foldl f False) bigSeq
    , bench "MSeq" $ whnf (foldl f False) bigMSeq
    , bench "Sequence" $ whnf (foldl f False) bigFTSeq
    , bench "RRBVector" $ whnf (foldl f False) bigRRB
    , bench "FingerTree" $ whnf (foldl f False) bigFT
    ]
  , bgroup "cps sum foldl" $
    let f k x = \acc -> if acc < 0 then acc else k (x+acc) in
    [ bench "Seq" $ whnf (\xs -> foldl f id xs 0) bigSeq
    , bench "MSeq" $ whnf (\xs -> foldl f id xs 0) bigMSeq
    , bench "Sequence" $ whnf (\xs -> foldl f id xs 0) bigFTSeq
    , bench "RRBVector" $ whnf (\xs -> foldl f id xs 0) bigRRB
    , bench "FingerTree" $ whnf (\xs -> foldl f id xs 0) bigFT
    ]
  , bgroup "sum . toList"
    [ bench "Seq" $ whnf (sum . F.toList) bigSeq
    , bench "MSeq" $ whnf (sum . F.toList) bigMSeq
    , bench "Sequence" $ whnf (sum . F.toList) bigFTSeq
    , bench "RRBVector" $ whnf (sum . F.toList) bigRRB
    , bench "FingerTree" $ whnf (sum . F.toList) bigFT
    ]
  , bgroup "foldl'"
    [ bench "Seq" $ whnf (F.foldl' (+) 0) bigSeq
    , bench "MSeq" $ whnf (F.foldl' (+) 0) bigMSeq
    , bench "Sequence" $ whnf (F.foldl' (+) 0) bigFTSeq
    , bench "RRBVector" $ whnf (F.foldl' (+) 0) bigRRB
    , bench "FingerTree" $ whnf (F.foldl' (+) 0) bigFT
    ]
  , bgroup "foldr'"
    [ bench "Seq" $ whnf (F.foldr' (+) 0) bigSeq
    , bench "MSeq" $ whnf (F.foldr' (+) 0) bigMSeq
    , bench "Sequence" $ whnf (F.foldr' (+) 0) bigFTSeq
    , bench "RRBVector" $ whnf (F.foldr' (+) 0) bigRRB
    , bench "FingerTree" $ whnf (F.foldr' (+) 0) bigFT
    ]
  , bgroup "toList"
    [ bench "Seq" $ whnf (foldr seq () . F.toList) bigSeq
    , bench "MSeq" $ whnf (foldr seq () . F.toList) bigMSeq
    , bench "Sequence" $ whnf (foldr seq () . F.toList) bigFTSeq
    , bench "RRBVector" $ whnf (foldr seq () . F.toList) bigRRB
    , bench "FingerTree" $ whnf (foldr seq () . F.toList) bigFT
    ]
  , bgroup "ifoldr short-circuit" $
    let f _ x z = x == (bigN `div` 2) || z in
    [ bench "Seq" $ whnf (IFo.ifoldr f False) bigSeq
    , bench "MSeq" $ whnf (IFo.ifoldr f False) bigMSeq
    , bench "Sequence" $ whnf (IFo.ifoldr f False) bigFTSeq
    , bench "RRBVector" $ whnf (IFo.ifoldr f False) bigRRB
    ]
  , bgroup "cps sum ifoldr" $
    let f _ x k = \acc -> if acc < 0 then acc else k (x+acc) in
    [ bench "Seq" $ whnf (\xs -> IFo.ifoldr f id xs 0) bigSeq
    , bench "MSeq" $ whnf (\xs -> IFo.ifoldr f id xs 0) bigMSeq
    , bench "Sequence" $ whnf (\xs -> IFo.ifoldr f id xs 0) bigFTSeq
    , bench "RRBVector" $ whnf (\xs -> IFo.ifoldr f id xs 0) bigRRB
    ]
  , bgroup "ifoldl short-circuit" $
    let f _ z x = x == (bigN `div` 2) || z in
    [ bench "Seq" $ whnf (IFo.ifoldl f False) bigSeq
    , bench "MSeq" $ whnf (IFo.ifoldl f False) bigMSeq
    , bench "Sequence" $ whnf (IFo.ifoldl f False) bigFTSeq
    , bench "RRBVector" $ whnf (IFo.ifoldl f False) bigRRB
    ]
  , bgroup "cps sum ifoldl" $
    let f _ k x = \acc -> if acc < 0 then acc else k (x+acc) in
    [ bench "Seq" $ whnf (\xs -> IFo.ifoldl f id xs 0) bigSeq
    , bench "MSeq" $ whnf (\xs -> IFo.ifoldl f id xs 0) bigMSeq
    , bench "Sequence" $ whnf (\xs -> IFo.ifoldl f id xs 0) bigFTSeq
    , bench "RRBVector" $ whnf (\xs -> IFo.ifoldl f id xs 0) bigRRB
    ]
  , bgroup "ifoldl'" $
    let f z i x = z + i + x in
    [ bench "Seq" $ whnf (IFo.ifoldl' f 0) bigSeq
    , bench "MSeq" $ whnf (IFo.ifoldl' f 0) bigMSeq
    , bench "Sequence" $ whnf (IFo.ifoldl' f 0) bigFTSeq
    , bench "RRBVector" $ whnf (IFo.ifoldl' f 0) bigRRB
    ]
  , bgroup "ifoldr'" $
    let f i x z = i + x + z in
    [ bench "Seq" $ whnf (IFo.ifoldr' f 0) bigSeq
    , bench "MSeq" $ whnf (IFo.ifoldr' f 0) bigMSeq
    , bench "Sequence" $ whnf (IFo.ifoldr' f 0) bigFTSeq
    , bench "RRBVector" $ whnf (IFo.ifoldr' f 0) bigRRB
    ]
  , bgroup "=="
    [ bench "Seq" $ whnf (join (==)) bigSeq
    , bench "MSeq" $ whnf (join (==)) bigMSeq
    , bench "Sequence" $ whnf (join (==)) bigFTSeq
    , bench "RRBVector" $ whnf (join (==)) bigRRB
    , bench "FingerTree" $ whnf (join (==)) bigFT
    ]
  , bgroup "compare"
    [ bench "Seq" $ whnf (join compare) bigSeq
    , bench "MSeq" $ whnf (join compare) bigMSeq
    , bench "Sequence" $ whnf (join compare) bigFTSeq
    , bench "RRBVector" $ whnf (join compare) bigRRB
    , bench "FingerTree" $ whnf (join compare) bigFT
    ]
  , bgroup "replicate"
    [ bench "Seq" $ whnf (`Seq.replicate` (0 :: Int)) bigN
    , bench "MSeq" $ whnf (`MSeq.replicate` (0 :: Int)) bigN
    , bench "Sequence" $ whnf (`FTSeq.replicate` (0 :: Int)) bigN
    , bench "RRBVector" $ whnf (`RRB.replicate` (0 :: Int)) bigN
    ]
  , bgroup "generate"
    [ bench "Seq" $ whnf (`Seq.generate` (\_ -> 0 :: Int)) bigN
    , bench "MSeq" $ whnf (`MSeq.generate` (\_ -> 0 :: Int)) bigN
    , bench "Sequence" $ whnf (`FTSeq.fromFunction` (\_ -> 0 :: Int)) bigN
    ]
  , bgroup "big <> big"
    [ bench "Seq" $ whnf (join (<>)) bigSeq
    , bench "MSeq" $ whnf (join (<>)) bigMSeq
    , bench "Sequence" $ whnf (join (<>)) bigFTSeq
    , bench "RRBVector" $ whnf (join (<>)) bigRRB
    , bench "FingerTree" $ whnf (join (<>)) bigFT
    ]
  , bgroup "big <> 2"
    [ bench "Seq" $ whnf (<> Seq.fromList [0,0]) bigSeq
    , bench "MSeq" $ whnf (<> MSeq.fromList [0,0]) bigMSeq
    , bench "Sequence" $ whnf (<> FTSeq.fromList [0,0]) bigFTSeq
    , bench "RRBVector" $ whnf (<> RRB.fromList [0,0]) bigRRB
    , bench "FingerTree" $ whnf (<> FT.fromList [0,0]) bigFT
    ]
  , bgroup "cons many"
    [ bench "Seq" $ whnf (F.foldl' (flip Seq.cons) Seq.empty) bigList
    , bench "MSeq" $ whnf (F.foldl' (flip MSeq.cons) MSeq.empty) bigList
    , bench "Sequence" $ whnf (F.foldl' (flip (FTSeq.<|)) FTSeq.empty) bigList
    , bench "RRBVector" $ whnf (F.foldl' (flip (RRB.<|)) RRB.empty) bigList
    , bench "FingerTree" $ whnf (F.foldl' (flip (FT.<|)) FT.empty) bigList
    ]
  , bgroup "snoc many"
    [ bench "Seq" $ whnf (F.foldl' Seq.snoc Seq.empty) bigList
    , bench "MSeq" $ whnf (F.foldl' MSeq.snoc MSeq.empty) bigList
    , bench "Sequence" $ whnf (F.foldl' (FTSeq.|>) FTSeq.empty) bigList
    , bench "RRBVector" $ whnf (F.foldl' (RRB.|>) RRB.empty) bigList
    , bench "FingerTree" $ whnf (F.foldl' (FT.|>) FT.empty) bigList
    ]
  , bgroup "index many"
    [ bench "Seq" $ whnf (F.foldr (\i z -> Seq.index i bigSeq `seq` z) ()) bigList
    , bench "MSeq" $ whnf (F.foldr (\i z -> MSeq.index i bigMSeq `seq` z) ()) bigList
    , bench "Sequence" $ whnf (F.foldr (\i z -> FTSeq.index bigFTSeq i `seq` z) ()) bigList
    , bench "RRBVector" $ whnf (F.foldr (\i z -> RRB.index i bigRRB `seq` z) ()) bigList
    ]
  , bgroup "lookup many"
    [ bench "Seq" $ whnf (F.foldr (\i z -> Seq.lookup i bigSeq `seq` z) ()) bigList
    , bench "MSeq" $ whnf (F.foldr (\i z -> MSeq.lookup i bigMSeq `seq` z) ()) bigList
    , bench "Sequence" $ whnf (F.foldr (\i z -> FTSeq.lookup i bigFTSeq `seq` z) ()) bigList
    , bench "RRBVector" $ whnf (F.foldr (\i z -> RRB.lookup i bigRRB `seq` z) ()) bigList
    ]
  , bgroup "update many"
    [ bench "Seq" $ whnf (F.foldl' (\xs1 i -> Seq.update i i xs1) bigSeq) bigList
    , bench "MSeq" $ whnf (F.foldl' (\xs1 i -> MSeq.update i i xs1) bigMSeq) bigList
    , bench "Sequence" $ whnf (F.foldl' (\xs1 i -> FTSeq.update i i xs1) bigFTSeq) bigList
    , bench "RRBVector" $ whnf (F.foldl' (\xs1 i -> RRB.update i i xs1) bigRRB) bigList
    ]
  , bgroup "insertAt many"
    [ bench "Seq" $ whnf (F.foldr (\i z -> Seq.insertAt i 0 bigSeq `seq` z) ()) bigList
    , bench "MSeq" $ whnf (F.foldr (\i z -> MSeq.insertAt i 0 bigMSeq `seq` z) ()) bigList
    , bench "Sequence" $ whnf (F.foldr (\i z -> FTSeq.insertAt i 0 bigFTSeq `seq` z) ()) bigList
    , bench "RRBVector" $ whnf (F.foldr (\i z -> RRB.insertAt i 0 bigRRB `seq` z) ()) bigList
    ]
  , bgroup "deleteAt many"
    [ bench "Seq" $ whnf (F.foldr (\i z -> Seq.deleteAt i bigSeq `seq` z) ()) bigList
    , bench "MSeq" $ whnf (F.foldr (\i z -> MSeq.deleteAt i bigMSeq `seq` z) ()) bigList
    , bench "Sequence" $ whnf (F.foldr (\i z -> FTSeq.deleteAt i bigFTSeq `seq` z) ()) bigList
    , bench "RRBVector" $ whnf (F.foldr (\i z -> RRB.deleteAt i bigRRB `seq` z) ()) bigList
    ]
  , bgroup "uncons many"
    [ bench "Seq" $ whnf (let go = maybe () (go . snd) . Seq.uncons in go) bigSeq
    , bench "MSeq" $ whnf (let go = maybe () (go . snd) . MSeq.uncons in go) bigMSeq
    , bench "Sequence" $ whnf (let go FTSeq.Empty = (); go (_ FTSeq.:<| xs1) = go xs1 in go) bigFTSeq
    , bench "RRBVector" $ whnf (let go = maybe () (go . snd) . RRB.viewl in go) bigRRB
    , bench "FingerTree" $ whnf (let go xs = case FT.viewl xs of FT.EmptyL -> (); _ FT.:< xs1 -> go xs1 in go) bigFT
    ]
  , bgroup "unsnoc many"
    [ bench "Seq" $ whnf (let go = maybe () (go . fst) . Seq.unsnoc in go) bigSeq
    , bench "MSeq" $ whnf (let go = maybe () (go . fst) . MSeq.unsnoc in go) bigMSeq
    , bench "Sequence" $ whnf (let go FTSeq.Empty = (); go (xs1 FTSeq.:|> _) = go xs1 in go) bigFTSeq
    , bench "RRBVector" $ whnf (let go = maybe () (go . fst) . RRB.viewr in go) bigRRB
    , bench "FingerTree" $ whnf (let go xs = case FT.viewr xs of FT.EmptyR -> (); xs1 FT.:> _ -> go xs1 in go) bigFT
    ]
  , bgroup "splitAt many"
    [ bench "Seq" $ whnf (F.foldr (\i z -> rwhnfTup2 (Seq.splitAt i bigSeq) `seq` z) ()) bigList
    , bench "MSeq" $ whnf (F.foldr (\i z -> rwhnfTup2 (MSeq.splitAt i bigMSeq) `seq` z) ()) bigList
    , bench "Sequence" $ whnf (F.foldr (\i z -> rwhnfTup2 (FTSeq.splitAt i bigFTSeq) `seq` z) ()) bigList
    , bench "RRBVector" $ whnf (F.foldr (\i z -> rwhnfTup2 (RRB.splitAt i bigRRB) `seq` z) ()) bigList
    ]
  , bgroup "tails"
    [ bench "Seq" $ whnf Seq.tails bigSeq
    , bench "Sequence" $ whnf FTSeq.tails bigFTSeq
    , bench "Sequence each" $ whnfAllFoldable FTSeq.tails bigFTSeq
    ]
  , bgroup "inits"
    [ bench "Seq" $ whnf Seq.inits bigSeq
    , bench "Sequence" $ whnf FTSeq.inits bigFTSeq
    , bench "Sequence each" $ whnfAllFoldable FTSeq.inits bigFTSeq
    ]
  , bgroup "chunks 2"
    [ bench "Seq" $ whnf (Seq.chunksOf 2) bigSeq
    , bench "Sequence" $ whnf (FTSeq.chunksOf 2) bigFTSeq
    ]
  , bgroup "chunks 2 nf"
    [ bench "Seq" $ nf (Seq.chunksOf 2) bigSeq
    , bench "Sequence" $ nf (FTSeq.chunksOf 2) bigFTSeq
    ]
  , bgroup "chunks sqrt"
    [ bench "Seq" $ whnf (Seq.chunksOf bigNSqrt) bigSeq
    , bench "Sequence" $ whnf (FTSeq.chunksOf bigNSqrt) bigFTSeq
    ]
  , bgroup "chunks sqrt nf"
    [ bench "Seq" $ nf (Seq.chunksOf bigNSqrt) bigSeq
    , bench "Sequence" $ nf (FTSeq.chunksOf bigNSqrt) bigFTSeq
    ]
  , bgroup "foldMap"
    [ bench "Seq" $ whnf (foldMap (\_ -> Unit)) bigSeq
    , bench "MSeq" $ whnf (foldMap (\_ -> Unit)) bigMSeq
    , bench "Sequence" $ whnf (foldMap (\_ -> Unit)) bigFTSeq
    , bench "RRBVector" $ whnf (foldMap (\_ -> Unit)) bigRRB
    , bench "FingerTree" $ whnf (foldMap (\_ -> Unit)) bigFT
    ]
  , bgroup "map"
    [ bench "Seq" $ whnf (\x -> fmap (\_ -> 0 :: Int) x) bigSeq
    , bench "MSeq" $ whnf (MSeq.map (\_ -> 0 :: Int)) bigMSeq
    , bench "Sequence" $ whnf (fmap (\_ -> 0 :: Int)) bigFTSeq
    , bench "RRBVector" $ whnf (fmap (\_ -> 0 :: Int)) bigRRB
    , bench "FingerTree" $ whnf (FT.fmap' (\_ -> 0 :: Int)) bigFT
    ]
  , bgroup "map nf"
    [ bench "Seq" $ nf (\x -> fmap (\_ -> 0 :: Int) x) bigSeq
    , bench "MSeq" $ nf (MSeq.map (\_ -> 0 :: Int)) bigMSeq
    , bench "Sequence" $ nf (fmap (\_ -> 0 :: Int)) bigFTSeq
    , bench "RRBVector" $ nf (fmap (\_ -> 0 :: Int)) bigRRB
    , bench "FingerTree" $ nfFoldable (FT.fmap' (\_ -> 0 :: Int)) bigFT
    ]
  , bgroup "imap"
    [ bench "Seq" $ whnf (IFu.imap (\_ _ -> 0 :: Int)) bigSeq
    , bench "MSeq" $ whnf (MSeq.imap (\_ _ -> 0 :: Int)) bigMSeq
    , bench "Sequence" $ whnf (FTSeq.mapWithIndex (\_ _ -> 0 :: Int)) bigFTSeq
    , bench "RRBVector" $ whnf (IFu.imap (\_ _ -> 0 :: Int)) bigRRB
    ]
  , bgroup "liftA2"
    [ bench "Seq" $ whnf (join $ liftA2 (\_ _ -> 0 :: Int)) bigSeq
    , bench "MSeq" $ whnf (join $ MSeq.liftA2 (\_ _ -> 0 :: Int)) bigMSeq
    , bench "Sequence" $ whnf (join $ liftA2 (\_ _ -> 0 :: Int)) bigFTSeq
    -- , bench "RRBVector" $ whnf (join $ liftA2 (\_ _ -> 0 :: Int)) bigRRB -- too slow!
    ]
  , bgroup "<*"
    [ bench "Seq" $ whnf (join (<*)) medSeq
    , bench "Sequence" $ whnf (join (<*)) medFTSeq
    , bench "RRBVector" $ whnf (join (<*)) medRRB
    ]
  , bgroup "*>"
    [ bench "Seq" $ whnf (join (*>)) medSeq
    , bench "Sequence" $ whnf (join (*>)) medFTSeq
    , bench "RRBVector" $ whnf (join (*>)) medRRB
    ]
  , bgroup ">>= 1"
    [ bench "Seq" $ whnf (\xs -> xs >>= \_ -> xs) bigSeq
    , bench "MSeq" $ whnf (\xs -> MSeq.concatMap (\_ -> xs) xs) bigMSeq
    , bench "Sequence" $ whnf (\xs -> xs >>= \_ -> xs) bigFTSeq
    , bench "RRBVector" $ whnf (\xs -> xs >>= \_ -> xs) bigRRB
    ]
  , bgroup ">>= 2"
    [ bench "Seq" $ whnf (\xs -> xs >>= \x -> if x `mod` 13 == 0 then bigSeq else pure x) bigSeq
    , bench "MSeq" $ whnf (\xs -> MSeq.concatMap (\x -> if x `mod` 13 == 0 then bigMSeq else MSeq.singleton x) xs) bigMSeq
    , bench "Sequence" $ whnf (\xs -> xs >>= \x -> if x `mod` 13 == 0 then bigFTSeq else pure x) bigFTSeq
    , bench "RRBVector" $ whnf (\xs -> xs >>= \x -> if x `mod` 13 == 0 then bigRRB else pure x) bigRRB
    ]
  , bgroup ">>= 3"
    [ bench "Seq" $ whnf (\xs -> xs >>= \x -> if x `mod` 13 == 0 then pure x else bigSeq) bigSeq
    , bench "MSeq" $ whnf (\xs -> MSeq.concatMap (\x -> if x `mod` 13 == 0 then MSeq.singleton x else bigMSeq) xs) bigMSeq
    , bench "Sequence" $ whnf (\xs -> xs >>= \x -> if x `mod` 13 == 0 then pure x else bigFTSeq) bigFTSeq
    , bench "RRBVector" $ whnf (\xs -> xs >>= \x -> if x `mod` 13 == 0 then pure x else bigRRB) bigRRB
    ]
  , bgroup "filter"
    [ bench "Seq" $ whnf (Seq.filter even) bigSeq
    , bench "MSeq" $ whnf (MSeq.filter even) bigMSeq
    , bench "Sequence" $ whnf (FTSeq.filter even) bigFTSeq
    ]
  , bgroup "mapMaybe"
    [ bench "Seq" $ whnf (Seq.mapMaybe (\x -> if even x then Just x else Nothing)) bigSeq
    , bench "MSeq" $ whnf (MSeq.mapMaybe (\x -> if even x then Just x else Nothing)) bigMSeq
    ]
  , bgroup "mapEither"
    [ bench "Seq" $ whnf (rwhnfTup2 . Seq.mapEither (\x -> if even x then Left x else Right x)) bigSeq
    , bench "MSeq" $ whnf (rwhnfTup2 . MSeq.mapEither (\x -> if even x then Left x else Right x)) bigMSeq
    ]
  , bgroup "span"
    [ bench "Seq" $ whnf (rwhnfTup2 . Seq.span (<bigNDiv2)) bigSeq
    , bench "MSeq" $ whnf (rwhnfTup2 . MSeq.span (<bigNDiv2)) bigMSeq
    , bench "Sequence" $ whnf (rwhnfTup2 . FTSeq.spanl (<bigNDiv2)) bigFTSeq
    ]
  , bgroup "findIndex"
    [ bench "Seq" $ whnf (Seq.findIndex (bigNDiv2 ==)) bigSeq
    , bench "MSeq" $ whnf (MSeq.findIndex (bigNDiv2 ==)) bigMSeq
    , bench "Sequence" $ whnf (FTSeq.findIndexL (bigNDiv2 ==)) bigFTSeq
    , bench "RRBVector" $ whnf (RRB.findIndexL (bigNDiv2 ==)) bigRRB
    ]
  , bgroup "reverse"
    [ bench "Seq" $ whnf Seq.reverse bigSeq
    , bench "MSeq" $ whnf MSeq.reverse bigMSeq
    , bench "Sequence" $ whnf FTSeq.reverse bigFTSeq
    , bench "FingerTree" $ whnf FT.reverse bigFT
    ]
  , bgroup "reverse nf"
    [ bench "Seq" $ nf Seq.reverse bigSeq
    , bench "MSeq" $ nf MSeq.reverse bigMSeq
    , bench "Sequence" $ nf FTSeq.reverse bigFTSeq
    , bench "FingerTree" $ nfFoldable FT.reverse bigFT
    ]
  , bgroup "intersperse"
    [ bench "Seq" $ whnf (Seq.intersperse 0) bigSeq
    , bench "MSeq" $ whnf (MSeq.intersperse 0) bigMSeq
    , bench "Sequence" $ whnf (FTSeq.intersperse 0) bigFTSeq
    ]
  , bgroup "intersperse nf"
    [ bench "Seq" $ nf (Seq.intersperse 0) bigSeq
    , bench "MSeq" $ nf (MSeq.intersperse 0) bigMSeq
    , bench "Sequence" $ nf (FTSeq.intersperse 0) bigFTSeq
    ]
  , bgroup "scanl"
    [ bench "Seq" $ whnf (Seq.scanl (\_ _ -> 0) (0 :: Int)) bigSeq
    , bench "MSeq" $ whnf (MSeq.scanl (\_ _ -> 0) (0 :: Int)) bigMSeq
    , bench "Sequence" $ whnf (FTSeq.scanl (\_ _ -> 0) (0 :: Int)) bigFTSeq
    ]
  , bgroup "scanl nf"
    [ bench "Seq" $ nf (Seq.scanl (\_ _ -> 0) (0 :: Int)) bigSeq
    , bench "MSeq" $ nf (MSeq.scanl (\_ _ -> 0) (0 :: Int)) bigMSeq
    , bench "Sequence" $ nf (FTSeq.scanl (\_ _ -> 0) (0 :: Int)) bigFTSeq
    ]
  , bgroup "scanr"
    [ bench "Seq" $ whnf (Seq.scanr (\_ _ -> 0) (0 :: Int)) bigSeq
    , bench "MSeq" $ whnf (MSeq.scanr (\_ _ -> 0) (0 :: Int)) bigMSeq
    , bench "Sequence" $ whnf (FTSeq.scanr (\_ _ -> 0) (0 :: Int)) bigFTSeq
    ]
  , bgroup "scanr nf"
    [ bench "Seq" $ nf (Seq.scanr (\_ _ -> 0) (0 :: Int)) bigSeq
    , bench "MSeq" $ nf (MSeq.scanr (\_ _ -> 0) (0 :: Int)) bigMSeq
    , bench "Sequence" $ nf (FTSeq.scanr (\_ _ -> 0) (0 :: Int)) bigFTSeq
    ]
  , bgroup "sort random"
    [ bench "Seq" $ whnf Seq.sort bigRandomSeq
    , bench "MSeq" $ whnf MSeq.sort bigRandomMSeq
    , bench "Sequence" $ whnf FTSeq.sort bigRandomFTSeq
    , bench "Sequence unstable" $ whnf FTSeq.unstableSort bigRandomFTSeq
    ]
  , bgroup "sort asc"
    [ bench "Seq" $ whnf Seq.sort bigSeq
    , bench "MSeq" $ whnf MSeq.sort bigMSeq
    , bench "Sequence" $ whnf FTSeq.sort bigFTSeq
    , bench "Sequence unstable" $ whnf FTSeq.unstableSort bigFTSeq
    ]
  , bgroup "zipWith"
    [ bench "Seq" $ whnf (\xs -> Seq.zipWith (\_ _ -> 0 :: Int) xs xs) bigSeq
    , bench "MSeq" $ whnf (\xs -> MSeq.zipWith (\_ _ -> 0 :: Int) xs xs) bigMSeq
    , bench "Sequence" $ whnf (\xs -> FTSeq.zipWith (\_ _ -> 0 :: Int) xs xs) bigFTSeq
    , bench "RRBVector" $ whnf (\xs -> RRB.zipWith (\_ _ -> 0 :: Int) xs xs) bigRRB
    ]
  , bgroup "zipWith nf"
    [ bench "Seq" $ nf (\xs -> Seq.zipWith (\_ _ -> 0 :: Int) xs xs) bigSeq
    , bench "MSeq" $ nf (\xs -> MSeq.zipWith (\_ _ -> 0 :: Int) xs xs) bigMSeq
    , bench "Sequence" $ nf (\xs -> FTSeq.zipWith (\_ _ -> 0 :: Int) xs xs) bigFTSeq
    , bench "RRBVector" $ nf (\xs -> RRB.zipWith (\_ _ -> 0 :: Int) xs xs) bigRRB
    ]
  , bgroup "zipWith3"
    [ bench "Seq" $ whnf (\xs -> Seq.zipWith3 (\_ _ _ -> 0 :: Int) xs xs xs) bigSeq
    , bench "MSeq" $ whnf (\xs -> MSeq.zipWith3 (\_ _ _ -> 0 :: Int) xs xs xs) bigMSeq
    , bench "Sequence" $ whnf (\xs -> FTSeq.zipWith3 (\_ _ _ -> 0 :: Int) xs xs xs) bigFTSeq
    ]
  , bgroup "zipWith3 nf"
    [ bench "Seq" $ nf (\xs -> Seq.zipWith3 (\_ _ _ -> 0 :: Int) xs xs xs) bigSeq
    , bench "MSeq" $ nf (\xs -> MSeq.zipWith3 (\_ _ _ -> 0 :: Int) xs xs xs) bigMSeq
    , bench "Sequence" $ nf (\xs -> FTSeq.zipWith3 (\_ _ _ -> 0 :: Int) xs xs xs) bigFTSeq
    ]
  , bgroup "unzipWith"
    [ bench "Seq" $ whnf (Seq.unzipWith (\x -> (x,x))) bigSeq
    , bench "MSeq" $ whnf (MSeq.unzipWith (\x -> (x,x))) bigMSeq
    , bench "Sequence" $ whnf (FTSeq.unzipWith (\x -> (x,x))) bigFTSeq
    , bench "RRBVector" $ whnf (RRB.unzipWith (\x -> (x,x))) bigRRB
    ]
  , bgroup "unzipWith nf"
    [ bench "Seq" $ nf (Seq.unzipWith (\x -> (x,x))) bigSeq
    , bench "MSeq" $ nf (MSeq.unzipWith (\x -> (x,x))) bigMSeq
    , bench "Sequence" $ nf (FTSeq.unzipWith (\x -> (x,x))) bigFTSeq
    , bench "RRBVector" $ nf (RRB.unzipWith (\x -> (x,x))) bigRRB
    ]
  , bgroup "mconcat"
    [ envp (map Seq.singleton bigList) $ \xs ->
      bench "Seq" $ whnf mconcat xs
    , envp (map FTSeq.singleton bigList) $ \xs ->
      bench "Sequence" $ whnf mconcat xs
    , envp (map RRB.singleton bigList) $ \xs ->
      bench "RRBVector" $ whnf mconcat xs
    ]
  , bgroup "binarySearchFind"
    [ bench "Seq" $ whnf (\(xs,is) -> foldr (\ !i z -> Seq.binarySearchFind (`compare` i) xs `seq` z) () is) (bigSeq, bigList)
    , bench "MSeq" $ whnf (\(xs,is) -> foldr (\ !i z -> MSeq.binarySearchFind (`compare` i) xs `seq` z) () is) (bigMSeq, bigList)
    ]
  , bgroup "isPrefixOf"
    [ bench "Seq" $ whnf (join Seq.isPrefixOf) bigSeq
    , bench "MSeq" $ whnf (join MSeq.isPrefixOf) bigMSeq
    ]
  , bgroup "infixIndices full"
    [ bench "Seq" $ nf (join Seq.infixIndices) bigSeq
    ]
  , bgroup "infixIndices all"
    [ bench "Seq" $ nf (Seq.infixIndices (Seq.singleton 0)) big0Seq
    ]
  , bgroup "binarySearchPrefix"
    [ bench "MSeq" $ whnf (foldr (\ !i z -> MSeq.binarySearchPrefix ((>=i) . getSum) bigMSeqS `seq` z) ()) bigList
    ]
  , bgroup "binarySearchSuffix"
    [ bench "MSeq" $ whnf (foldr (\ !i z -> MSeq.binarySearchSuffix ((>=i) . getSum) bigMSeqS `seq` z) ()) bigList
    ]
  , bgroup "measured split"
    [ bench "MSeq" $
        let spl p xs = case MSeq.binarySearchPrefix p xs of
                         (Nothing, _) -> (xs, MSeq.empty)
                         (Just i, _) -> MSeq.splitAt (i+1) xs
        in
        whnf (foldr (\ !i z -> rwhnfTup2 (spl ((>=i) . getSum) bigMSeqS) `seq` z) ()) bigList
    , bench "FingerTree" $
        whnf (foldr (\ !i z -> rwhnfTup2 (FT.split ((>=i) . getSum) bigFTS) `seq` z) ()) bigList
    ]
  ]

pqBenches :: Benchmark
pqBenches = envp pqData $ \_ -> bgroup "PQueue"
  [ bgroup "fromList"
    [ bench "seqn" $ whnf (PQ.fromList . map (uncurry PQ.Entry)) bigRandomList2
    , bench "pqueue" $ whnf PQPM.fromList bigRandomList2
    , bench "fingertree" $ whnf FTPQ.fromList bigRandomList2
    ]
  , bgroup "insert"
    [ bench "seqn" $ whnf (F.foldl' (\acc (k,x) -> PQ.insert (PQ.Entry k x) acc) PQ.empty) bigRandomList2
    , bench "pqueue" $ whnf (F.foldl' (\acc (k,x) -> PQPM.insert k x acc) PQPM.empty) bigRandomList2
    , bench "fingertree" $ whnf (F.foldl' (\acc (k,x) -> FTPQ.insert k x acc) FTPQ.empty) bigRandomList2
    ]
  , bgroup "minView"
    [ bench "seqn" $ whnf (until null (\q -> case PQ.minView q of Just (_,q') -> q')) bigPQ
    , bench "pqueue" $ whnf (until PQPM.null PQPM.deleteMin) bigPQPM
    , bench "fingertree" $ whnf (until null (\q -> case FTPQ.minView q of Just (_,q') -> q')) bigFTPQ
    ]
  , bgroup "sort list"
    [ bench "seqn" $ whnf (length . PQ.toSortedList . PQ.fromList . map (uncurry PQ.Entry)) bigRandomList2
    , bench "pqueue" $ whnf (length . PQPM.toAscList . PQPM.fromList) bigRandomList2
    , bench "fingertree" $ whnf (length . L.unfoldr FTPQ.minViewWithKey . FTPQ.fromList) bigRandomList2
    ]
  ]

----------
-- Utils
----------

whnfAllFoldable :: (NFData b, Foldable f) => (a -> f b) -> a -> Benchmarkable
whnfAllFoldable f = whnf (\x -> let y = f x in rwhnfAllFoldable y `seq` y)

-- This is used for FingerTree because it does not define NFData. Yes, it
-- does not force the measures. I don't see any way to force both value and
-- measure. Internals are not exposed either.
nfFoldable :: (NFData b, Foldable f) => (a -> f b) -> a -> Benchmarkable
nfFoldable f = whnf (\x -> let y = f x in rnfFoldable y `seq` y)

rwhnfTup2 :: (a, b) -> ()
rwhnfTup2 (!_, !_)  = ()

envp :: NFData e => e -> (e -> Benchmark) -> Benchmark
envp = env . pure

rnfFoldable :: (NFData b, Foldable f) => f b -> ()
rnfFoldable x = foldMap (`deepseq` Unit) x `seq` ()

rwhnfAllFoldable :: (NFData a, Foldable f) => f a -> ()
rwhnfAllFoldable x = foldMap (`seq` Unit) x `seq` ()

data Unit = Unit

instance Semigroup Unit where
  Unit <> Unit = Unit

instance Monoid Unit where
  mempty = Unit

newtype WHNF a = WHNF a

instance NFData (WHNF a) where
  rnf = rwhnf

-------------
-- All data
-------------

seqData =
  ( bigList
  , ( bigSeq
    , big0Seq
    , bigRandomSeq
    , medSeq
    )
  , ( bigMSeq
    , bigRandomMSeq
    , bigMSeqS
    )
  , ( bigFTSeq
    , medFTSeq
    , bigRandomFTSeq
    )
  , ( bigRRB
    , medRRB
    )
  , ( WHNF (rnfFoldable bigFT `seq` bigFT)
    )
  )

pqData =
  ( bigRandomList2
  , bigPQ
  , bigPQPM
  , WHNF (rnfFoldable bigFTPQ `seq` bigFTPQ)
  )

----------------
-- N and lists
----------------

bigN :: Int
bigN = 10000

bigNDiv2 :: Int
bigNDiv2 = bigN `div` 2

bigNSqrt :: Int
bigNSqrt = 70

bigList :: [Int]
bigList = [0 .. bigN-1]

medN :: Int
medN = 1000

medList :: [Int]
medList = [0 .. medN-1]

--------
-- Seq
--------

bigSeq, big0Seq, bigRandomSeq, medSeq :: Seq.Seq Int
bigSeq = Seq.fromList bigList
big0Seq = Seq.fromList big0List
bigRandomSeq = Seq.fromList bigRandomList
medSeq = Seq.fromList medList

---------
-- MSeq
---------

bigMSeq, bigRandomMSeq :: MSeq.MSeq Int
bigMSeq = MSeq.fromList bigList
bigRandomMSeq = MSeq.fromList bigRandomList

bigMSeqS :: MSeq.MSeq S
bigMSeqS = MSeq.fromList (replicate bigN (S 1))

-------------
-- Sequence
-------------

bigFTSeq, bigRandomFTSeq, medFTSeq :: FTSeq.Seq Int
bigFTSeq = FTSeq.fromList bigList
bigRandomFTSeq = FTSeq.fromList bigRandomList
medFTSeq = FTSeq.fromList medList

--------------
-- RRBVector
--------------

bigRRB, medRRB :: RRB.Vector Int
bigRRB = RRB.fromList bigList
medRRB = RRB.fromList medList

---------------
-- FingerTree
---------------

bigFT :: FT.FingerTree () Int
bigFT = FT.fromList bigList

bigFTS :: FT.FingerTree (Sum Int) S
bigFTS = FT.fromList (replicate bigN (S 1))

-----------
-- Random
-----------

bigRandomList :: [Int]
bigRandomList = randomInts bigN

big0List :: [Int]
big0List = replicate bigN 0

-- LCG
randomInts :: Int -> [Int]
randomInts n =
  take n $ L.iterate' (\i -> 0xffffffff .&. (i * 1103515245 + 12345)) n
{-# INLINE randomInts #-}

-----------
-- PQueue
-----------

bigRandomList2 :: [(Int,Int)]
bigRandomList2 = uncurry zip $ splitAt bigN $ randomInts (2*bigN)

bigPQ :: PQ.PQueue (PQ.Entry Int Int)
bigPQ = PQ.fromList [PQ.Entry k x | (k,x) <- bigRandomList2]

bigPQPM :: PQPM.MinPQueue Int Int
bigPQPM = PQPM.fromList bigRandomList2

bigFTPQ :: FTPQ.PQueue Int Int
bigFTPQ = FTPQ.fromList bigRandomList2

-------------
-- Measured
-------------

instance FT.Measured () Int where
  measure _ = ()

instance MSeq.Measured Int where
  type Measure Int = ()
  measure _ = ()

newtype S = S Int
  deriving newtype (Show, NFData)

instance FT.Measured (Sum Int) S where
  measure (S x) = Sum x

instance MSeq.Measured S where
  type Measure S = Sum Int
  measure (S x) = Sum x
