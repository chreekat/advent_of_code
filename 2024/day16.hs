{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant map" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use if" #-}
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

import Prelude hiding (lookup)
import Array qualified
import System.IO qualified as Sys
import Control.Monad qualified as Monad
import Control.Monad.State qualified as State
import Control.Monad.Writer.CPS as Writer
import Data.Foldable qualified as Fold
import Data.List qualified as List
import Data.List.Split qualified as Split
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Vector qualified as Vector
import HMap qualified
import Map qualified
import Seq qualified
import Set qualified
import Tropes hiding (traceShow, traceShowId, range,(!))
import Tropes qualified
import TwoD qualified

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day16-ex1.txt")
{-# NOINLINE ex2 #-}
ex2 = unsafePerformIO (readFile "day16-ex2.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day16.txt")

ans1 :: _
ans1 = let
    grid = TwoD.twoD dat
    Just start = Array.find (== 'S') grid
    Just goal = Array.find (== 'E') grid
    open = insert (Index start It) (N 0 0 It) empty
    -- in maybe "" showStep $ runAStar <=< runAStar <=< runAStar <=< runAStar $ AStar open empty goal grid
    in AStar open empty goal grid []

showStep :: AStar -> String
showStep (AStar open _ goal grid _) =
    let openIdxs = map snd (Set.toList (tl_set open))
    in TwoD.showTwoD (grid Array.// ((goal, 'A') : [ (i_pos i,'!') | i <- openIdxs ]))

showPath :: AStar -> _ -> String
showPath astar path = TwoD.showTwoD (a_grid astar Array.// [ (i ,'O') | i <- path ])

ans2 :: _
ans2 = undefined

main1 = do 
    Sys.hSetBuffering Sys.stdout Sys.NoBuffering
    Sys.hSetBuffering Sys.stdin Sys.NoBuffering
    let a = ans1
    let f s = do
            -- _ <- getChar
            case runAStar s of
                Nothing -> 
                    print $ Map.filterWithKey (\(Index p _) _ -> p == (7,5)) $ tl_map $ a_closed s
                    -- print (a_solns s, Map.size $ tl_map $ a_closed s)
                Just a' -> do
                    -- putStrLn $ showStep a'
                    f a'
    f a

main = do 
    Sys.hSetBuffering Sys.stdout Sys.NoBuffering
    Sys.hSetBuffering Sys.stdin Sys.NoBuffering
    let a = ans1
    let f s = do
            -- _ <- getChar
            case runAStar s of
                Nothing -> 
                    let paths = findAllPath s
                    in print $ (length $ paths, length $ nub $ sort paths)
                    --putStr $ showPath s $ findAllPath s
                    -- print (a_solns s, Map.size $ tl_map $ a_closed s)
                Just a' -> do
                    -- putStrLn $ showStep a'
                    f a'
    f a

-- Look for previous steps that come through us.
-- We know the position of previous steps. There may be multiple nodes at that
-- position. We pick all of them if their_cost + diff = our_cost.
backtrack (Index (x,y) d) astar =
    let prev_pos = case d of
            Po -> (x+1, y)
            Et -> (x-1, y)
            Lä -> (x, y+1)
            It -> (x, y-1)
        our_cost = g $ a_closed astar ! Index (x,y) d
        prev_pos_nodes = Map.filterWithKey filterPrev $ tl_map $ a_closed astar
        filterPrev (Index p d') n =
            p == prev_pos 
            && our_cost == g n + if d == d' then 1 else 1001
    in Map.keys $ prev_pos_nodes

-- findPath :: AStar -> Index (Int,Int) -> [(Int,Int)]
findPath a i = go a [i]
    where
    go _ [] = []
    go astar (i@(Index p d):rest) = p : case backtrack i astar of
        [] -> go astar rest
        -- Remove us from the list of closed paths before continuing
        -- Thread the new astar through every call: it's a fold
        prevs -> go (astar { a_closed = delete i (a_closed astar) }) (prevs ++ rest)

findAllPath astar =
    let endNodes = Map.filterWithKey filterKey $ tl_map $ a_closed astar
        lowestCost = minimum $ map g $ Map.elems endNodes
        allSuccess = Map.filter (\n -> g n == lowestCost) endNodes
        filterKey (Index p _) _ = p == a_goal astar
    in concatMap (findPath astar) (Map.keys allSuccess)

-- A* algorithm
--
runAStar astar = do
    (q@(i,node), open') <- least (a_open astar)
    let closed' = insert i node (a_closed astar)
    let neighbors' = neighbors i (g node + 1 ) astar
    let open'' = foldr replaceBigger open' neighbors'
    let solns' = (if i_pos i == a_goal astar then (q :) else id) (a_solns astar)
    pure $ astar { a_open = open'', a_closed = closed', a_solns = solns' }

heuristic :: (Int,Int) -> Index (Int,Int) -> Int
heuristic goal (Index i dir) =
    let (x, y) = i
        (x', y') = goal
        x_diff = abs (x - x')
        y_diff = abs (y - y')
        -- Turn cost
        -- When on the top row, cost can be 0, 1000, or 2000 if facing east,
        -- south, or west respectively.
        --
        -- When on the right row, cost can be 0, 1000, or 2000 if facing north,
        -- west, or south respectively
        --
        -- When not on the top or right row, cost can be 1000 or 2000. 1000 if
        -- facing east or north, 2000 otherwise.
        turnCost
            | dir `elem` [Et,Lä]
            = 2000

            | x_diff == 0 , dir == It
            = 0

            | y_diff == 0 , dir == Po
            = 0

            | otherwise = 1000


    in turnCost + abs (x - x') + abs (y - y')

data AStar = AStar
    { a_open :: TwoLevel (Index (Int, Int)) Int N
    , a_closed :: TwoLevel (Index (Int, Int)) Int N
    , a_goal :: (Int, Int)
    , a_grid :: TwoD.TwoD Char
    , a_solns :: [(Index (Int, Int), N)]
} deriving (Eq, Show)

-- Two level map so I can look up by position or by cost.
data TwoLevel pos cost a = TwoLevel 
    { tl_map :: Map.Map pos a
    , tl_set :: Set.Set (cost,pos)
    } deriving (Eq, Show, Functor)

empty = TwoLevel Map.empty Set.empty

lookup pos (TwoLevel m _) = Map.lookup pos m
member pos (TwoLevel m _) = Map.member pos m
delete pos tl@(TwoLevel m s) = fromMaybe tl $ do
    n <- Map.lookup pos m
    let s' = Set.delete (g n + h n, pos) s
        m' = Map.delete pos m
    pure $ TwoLevel m' s'

(TwoLevel m _) ! pos = m Map.! pos

least :: Ord i => TwoLevel i j a -> Maybe ((i, a), TwoLevel i j a)
least (TwoLevel m s) = do
    ((_,i), srest) <- Set.minView s
    n <- Map.lookup i m
    let mrest = Map.delete i m
    pure ((i, n), TwoLevel mrest srest)

neighbors (Index (x,y) d) g' astar =
    let idxs = mapMaybe filterIdx
            [ Index (x + 1, y) Et
            , Index (x - 1, y) Po
            , Index (x, y + 1) It
            , Index (x, y - 1) Lä
            ]
        hs = map (heuristic (a_goal astar)) idxs
        gs = map (\(Index _ d') -> g' + if d == d' then 0 else 1000) idxs
    in zip idxs $ zipWith3 N gs hs (map i_dir idxs) 
    where
    filterIdx i@(Index p d')
        | Array.inRange (Array.bounds $ a_grid astar) p
        , not $ member i (a_closed astar)
        , a_grid astar Array.! p /= '#'
        , d /= revDir d'
        = Just i
        | otherwise = Nothing

    
-- FIXME: n_dir is probably unused
data N = N {g :: Int, h :: Int, n_dir :: Direction}
    deriving (Eq, Show)

data Direction = Po | It | Et | Lä
    deriving (Eq, Show, Ord)

revDir Po = Et
revDir It = Lä
revDir Et = Po
revDir Lä = It

-- Our actual index is position + direction, since there can be very different
-- costs for a position depending on which way we came from. They have to be
-- distinguished.
data Index pos = Index { i_pos :: pos, i_dir :: Direction }
    deriving (Eq, Show)

deriving instance Ord pos => Ord (Index pos)

-- Add an N to a TwoLevel
--
-- Take care to clean up the reverse map.
insert i node (TwoLevel m s) = TwoLevel
    (Map.insert i node m)
    (Set.insert (g node + h node, i)
        (case Map.lookup i m of
            Just n -> Set.delete (g n + h n, i) s
            Nothing -> s))

replaceBigger (i,node) tl = case lookup i tl of
    Just n | g n <= g node -> tl
    _ -> insert i node tl
