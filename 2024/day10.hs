{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant map" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Array qualified
import Control.Monad.Writer.CPS as Writer
import Data.Foldable qualified as Fold
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import HMap qualified
import Map qualified
import Seq qualified
import Tropes hiding (traceShow, traceShowId)
import Tropes qualified
import TwoD qualified

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day10-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day10.txt")

ans1 :: _
ans1 = 
    let m = TwoD.twoD' Tropes.digit dat
        (start, end) = recordStartEnd m
        mkStart i = (i, ([i],[])) <$ Writer.tell (Seq.singleton i)
        mkDest i = (i, ([],[i])) <$ Writer.tell (Seq.singleton i)
        (opens,idxs) = Writer.runWriter $ do
            ss <- mapM mkStart start
            dd <- mapM mkDest end
            pure $ Map.fromList ss <> Map.fromList dd
        --paths = run m opens idxs
    in length $ run m opens idxs

-- BFS search, bidirectional. Keep track of where we started or where we came
-- from.

-- Talking through the up case.
-- We have a queue of indexes to walk.
-- For each index,
--
-- If we have srcs and dests, generate paths from them and stop.
-- Otherwise,
--
-- Look up the uphill neighbors in the set of open nodes.
-- For each neighbor,
--
-- If the neighbor exists, add our srcs to their srcs.
-- If not, create the neighbor and add its index to the back of the queue.
--
-- For the down case, it's the same, except look for downhill neighbors.
type I = (Int,Int)

run map_ = fix f mempty where
    f _   paths _     Seq.Empty = paths
    f nxt paths opens (i :<| idxs) = case opens Map.! i of
        -- going up
        node@(_, []) -> 
            let uphills = uphill map_ i
                (newIdxs,newOpens) = foldl' (foldNeighbor node) (idxs, opens) uphills
            in nxt paths newOpens newIdxs
        -- going down
        node@([],_) -> 
            let downhills = downhill map_ i
                (newIdxs,newOpens) = foldl' (foldNeighbor node) (idxs, opens) downhills
            in nxt paths newOpens newIdxs
        -- met in the middle!
        (srcs,dests) ->
            let newPaths = [ (s,d) | s <- srcs, d <- dests ]
            in nxt (newPaths <> paths) opens idxs
        where
        foldNeighbor node (idxs', opens') j =
            (idxs' Seq.>< if j `Map.member` opens' then Seq.Empty else Seq.singleton j
            , Map.alter (combine node) j opens')

combine node Nothing = Just node
combine (srcs1,dests1) (Just (srcs2,dests2)) = Just (srcs1 <> srcs2, dests1 <> dests2)

uphill map_ i = filter (\j -> map_ Array.! j == map_ Array.! i + 1) (TwoD.neighbors map_ i)
downhill map_ i = filter (\j -> map_ Array.! j == map_ Array.! i - 1) (TwoD.neighbors map_ i)

recordStartEnd = Tropes.fix f ([],[]) . Array.assocs where
    f _ record [] = record
    f nxt (start, end) (x:xs) = case x of
        (i, 0) -> nxt (i:start, end) xs
        (i, 9) -> nxt (start, i:end) xs
        (_, _) -> nxt (start, end) xs

ans2 :: _

ans2 = undefined

main = print ans1
