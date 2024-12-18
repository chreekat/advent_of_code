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
import Control.Monad.State qualified as State
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
ex1 = unsafePerformIO (readFile "day12-ex1.txt")
{-# NOINLINE ex2 #-}
ex2 = unsafePerformIO (readFile "day12-ex2.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day12.txt")

ans1 :: _
ans1 = sum $ map (\(a,b,c) -> b*c) $ results $ process $ TwoD.twoD dat
--ans1 = results $ process $ TwoD.twoD ex1

-- 1. keep a map of unprocessed (open) nodes
-- 2. keep a reminder of what node type is currently being processed
-- 3. keep a running total for the current node type (area and edge length)
-- 4. keep a queue of nodes being processed
--
-- the queue of nodes being processed will have the same node type, so we don't
-- really need the 'reminder'
--
-- to process a node:
-- 1. add size and edges to running total for current node type
-- 2. preprocess neighbors with same node type
-- 
-- to preprocess a neighbor:
-- 1. If different node type, skip
-- 2. If neighbor is missing from the map of unprocessed nodes, skip
-- 3. Remove from map of unprocessed nodes and add to queue of nodes being
--    processed
--
-- to process a node type:
-- 1. if the queue is empty, return 
-- 2. otherwise, pop a node from the queue and process it
-- 
-- to process the graph:
-- 1. process nodes types until the open map is empty.
-- 2. return [(type, area, edge length)]

data S k v = S
    { open :: Map.Map k v
    , queue :: [(k, v)]
    , results :: [(Char, Int, Int)]
    , original :: TwoD.TwoD Char
    } deriving (Eq, Show)

oopen m = State.modify' (\s -> s {open = m})
qqueue m = State.modify' (\s -> s {queue = m})
rresults m = State.modify' (\s -> s {results = m})


type M k v = State.MonadState (S k v)

minViewWithKeyS :: M k v m => Map.Map k v -> m (Maybe (k,v))
minViewWithKeyS m = do
    case Map.minViewWithKey m of
        Nothing -> return Nothing
        Just (a, m') -> do
            oopen m'
            return (Just a)

process twoD = State.execState processGraph (S open' [] [] twoD)
  where
    open' = Map.fromList $ Array.assocs twoD

processGraph :: M (Int,Int) Char m => m [(Char, Int, Int)]
processGraph = do
    nextType <- minViewWithKeyS =<< gets open
    case nextType of
        Nothing -> gets results
        Just (k, v) -> do
            qqueue [(k, v)]
            State.modify' (\(S o q r orig) -> S o q ((v, 0, 0):r) orig)
            processNodeType
            processGraph

processNodeType :: M (Int,Int) Char m => m ()
processNodeType = do
    que <- gets queue
    case que of
        [] -> return ()
        (k, v):rest -> do
            qqueue rest
            processNode k v
            processNodeType

addNodeResult k = do
    -- avoid MonadFail
    res <- gets results
    orig <- gets original
    let ((v, area, edge):rest) = res
    -- nw = True if the TwoD.luode node is same type as the target node
    -- w = True for TwoD.west, etc
    let nw = maybe False ((== v) . (orig Array.!)) (TwoD.luode   k orig)
        w  = maybe False ((== v) . (orig Array.!)) (TwoD.west    k orig)
        n  = maybe False ((== v) . (orig Array.!)) (TwoD.north   k orig)
        e  = maybe False ((== v) . (orig Array.!)) (TwoD.east    k orig)
        s  = maybe False ((== v) . (orig Array.!)) (TwoD.south   k orig)
        ne = maybe False ((== v) . (orig Array.!)) (TwoD.koillis  k orig)
        sw = maybe False ((== v) . (orig Array.!)) (TwoD.lounas  k orig)

        leftEdge = not w && (not n || nw)
        rightEdge = not e && (not n || ne)
        topEdge = not n && (not w || nw)
        bottomEdge = not s && (not w || sw)
        edges = [topEdge, rightEdge, bottomEdge, leftEdge]
        edge' = length $ filter id edges
    let newRes = (v, area + 1, edge + edge'):rest
    (if False && v == 'C' then Tropes.traceShow (k,edges) else id) $ rresults newRes

processNode k v = do
    m <- gets open
    orig <- gets original
    let neighIdxs = TwoD.neighbors k orig
        sameTypeNIdxs = filter (\ni -> v == orig Array.! ni) neighIdxs
        unprocessedNs = mapMaybe (\i -> (i,) <$> Map.lookup i m) sameTypeNIdxs
        newOpen = foldr Map.delete m sameTypeNIdxs
    State.modify' (\(S _ q r _) -> S newOpen (q ++ unprocessedNs) r orig)
    addNodeResult k




ans2 :: _
ans2 = undefined

main = print ans1
