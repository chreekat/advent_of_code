{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant map" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use infix" #-}
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
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence qualified as Seq
import Data.Vector qualified as Vector
import TwoD qualified
import Tropes hiding (traceShow, traceShowId)
import Tropes qualified
import HMap qualified

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day6-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day6.txt")

inp1 = dat

ans1 :: _
ans1 =
    let map_ = TwoD.twoD' cell inp1
        Just start = Array.find isPath map_
    in show $ length $ Array.findAll isPath $ walk start map_

isPath (Paths _) = True
isPath _ = False

data Cell = Empty | Thing | Paths [Path]
    deriving (Show, Eq)

data Path = P Dir Dir
    deriving (Show, Eq)

data Dir = N | S | E | W | O
    deriving (Show, Eq)

printCell c = case c of
    Empty -> '.'
    Thing -> '#'
    Paths [P N O] -> '^'
    Paths [P S O] -> 'v'
    Paths [P E O] -> '>'
    Paths [P W O] -> '<'
    Paths [P N N] -> '↑'
    Paths [P N E] -> '↱'
    Paths [P E E] -> '→'
    Paths [P E S] -> '↴'
    Paths [P S S] -> '↓'
    Paths [P S W] -> '↵'
    Paths [P W W] -> '←'
    Paths [P W N] -> '⮤'
    _ -> error "printCell"

walk :: (Int, Int) -> TwoD.TwoD Cell -> TwoD.TwoD Cell
walk i@(i1,i2) map_ =
    let Paths [P in_ out] = map_ Array.! i
        j@(j1,j2) = case out of
            N -> (-1,0)
            S -> (1,0)
            E -> (0,1)
            W -> (0,-1)
            _ -> error ("j: " <> show i)
        nxt = (i1+j1,i2+j2)
        turn = case out of
            N -> E
            S -> W
            E -> S
            W -> N
            _ -> error "turn"
        next = Array.lookup nxt map_
        stopped = map_
        turned = map_ Array.// [(i,Paths [P in_ turn])]
        walked = map_ Array.// [(nxt,Paths [P out out])]
    in case next of
        Nothing -> stopped
        Just Empty -> walk nxt walked
        Just (Paths _) -> walk nxt walked
        Just Thing -> walk i turned

traceShow x y = y
traceShowId = id

-- If nxt already has a path coming from our direction, we've found a loop.
-- If we exit the map, we haven't.
findLoop i@(i1,i2) map_ extra =
    let Paths (P in_ out:others) = traceShow (i, map_ HMap.! i) $ map_ HMap.! i
        j@(j1,j2) = case out of
            N -> (-1,0)
            S -> (1,0)
            E -> (0,1)
            W -> (0,-1)
            _ -> error ("j: " <> show i)
        nxt = (i1+j1,i2+j2)
        cameFrom = case out of
            N -> S
            S -> N
            E -> W
            W -> E
            _ -> error "cameFrom"
        turn = case out of
            N -> E
            S -> W
            E -> S
            W -> N
            _ -> error "turn"
        next = HMap.lookup nxt map_
        haveLoop = [Just i]
        stopped = [Nothing]
        turned = map_ HMap.// [(i,Paths (P in_ turn:others))]
        walked oldPaths = map_ HMap.// [(nxt,Paths (P cameFrom out : oldPaths))]
        blocked = turned HMap.// [(nxt,Thing)]
        P in_ _ `comesFrom` i = in_ == cameFrom
    in case next of
        Nothing -> stopped
        Just Thing -> findLoop i turned extra
        Just Empty -> findLoop nxt (walked []) extra
            <> if extra then findLoop i blocked False else []
        Just (Paths ps)
            | any (`comesFrom` i) ps -> haveLoop
            | otherwise -> findLoop nxt (walked ps) extra


cell c = case c of
    '.' -> Empty
    '#' -> Thing
    '^' -> Paths [P O N]
    'v' -> Paths [P O S]
    '>' -> Paths [P O E]
    '<' -> Paths [P O W]
    _ -> error "cell"

-- now just literally try every example of replacing a Empty with a Thing and
-- collect results. So let's make a list of potential maps
allMaps map_ =
    let empties = HMap.findAll (==Empty) map_
    in map (\i -> HMap.insert (traceShowIdd i) Thing map_) empties

traceShowIdd = id

ans2 :: _
ans2 =
    let map_ = HMap.fromList (Array.assocs $TwoD.twoD' cell inp2)
        Just start = HMap.find isPath map_
    -- in show $ length $ mapMaybe (findLoop start) (allMaps map_)
    in show $ length $ catMaybes $ findLoop start map_ True

inp2 = dat
main = putStrLn ans2
