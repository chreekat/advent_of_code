{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Redundant map" #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# LANGUAGE LambdaCase #-}

import Tropes hiding ((!))
import Data.Array
import TwoD

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day10-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day10.txt")

data Cell = Cell Char (Maybe Int)
    deriving (Show, Eq)

pc (Cell c _) = c

pn (Cell c n) = n

g `gchar` p = pc (g ! p)

g `gnum` p = pn (g ! p)

fromMe g p@(x,y) = case g `gchar` p of
    '|' -> [(x-1,y),(x+1,y)]
    '-' -> [(x,y-1),(x,y+1)]
    'L' -> [(x-1,y),(x,y+1)]
    'J' -> [(x-1,y),(x,y-1)]
    '7' -> [(x+1,y),(x,y-1)]
    'F' -> [(x+1,y),(x,y+1)]
    _   -> []


intoMe g p@(x,y)
    =  [ (x-1,y) | g `gchar` (x-1,y) `elem` "|7F" ]
    ++ [ (x+1,y) | g `gchar` (x+1,y) `elem` "|LJ" ]
    ++ [ (x,y-1) | g `gchar` (x,y-1) `elem` "-FL" ]
    ++ [ (x,y+1) | g `gchar` (x,y+1) `elem` "-J7" ]

-- Start index is the index of the element that is an 'S'
start :: Array (Int, Int) Cell -> (Int, Int)
start = fst . head . filter ((== 'S') . pc . snd) . assocs


traceLoop g ptr n = g // [(ptr, Cell (g `gchar` ptr) (Just n))]

step g [] = g
step g (p:ps) =
    let neighs = fromMe g p
        minDist = 1 + minimum (mapMaybe (g `gnum`) neighs)
    in step (traceLoop g p minDist) (filter (maybe True (>= minDist) . (g `gnum`)) neighs ++ ps)

toLineDrawing = \case
    '|' -> '│'
    '-' -> '─'
    'L' -> '└'
    'J' -> '┘'
    '7' -> '┐'
    'F' -> '┌'
    'S' -> '╳'

ans1 :: _
ans1 = let g = twoD' (`Cell` Nothing) ex1
           s = start g
        in showTwoD (\pp -> maybe '🮕' (const $ toLineDrawing $ pc pp) (pn pp)) $ traceLoop g s 0 `step` intoMe g s


data Loc = In | Out
    deriving (Eq, Show)

printLoc In = '🯅'
printLoc Out = ' '

switch In = Out
switch Out = In

crossBorder = \case
    '|' -> switch
    '-' -> id
    'L' -> id
    'J' -> id
    '7' -> switch
    'F' -> switch

isBorder g ray =
    case g `gnum` ray of
        Nothing -> False
        Just _ -> True

cast (g, cnt, loc) ray =
    let cnt' = cnt + (
            if isBorder g ray || loc == Out
                then 0
                else 1)
        loc' | isBorder g ray = crossBorder (g `gchar` ray) loc
             | otherwise = loc
    in (g, cnt', loc')

castRow g = _2of3 . foldl' cast (g, 0, Out)

coverGraph g = sum . map (castRow g) $ listOfListOfIndices g

whatAmI g s =
    let neighs = map (*- s) $ intoMe g s
        (x,y) *- (x', y') = (x-x', y-y')
    in case neighs of
        [(-1,0),(1,0)]  -> Cell '|' (Just 0)
        [(0,-1),(0,1)]  -> Cell '-' (Just 0)
        [(-1,0),(0,1)]  -> Cell 'L' (Just 0)
        [(-1,0),(0,-1)] -> Cell 'J' (Just 0)
        [(1,0),(0,1)]   -> Cell 'F' (Just 0)
        [(0,-1),(1,0)]  -> Cell '7' (Just 0)

ans2 :: _
ans2 = let g = twoD' (`Cell` Nothing) dat
           s = start g
           cleanG = g // [(s, whatAmI g s)]
           fullMap = traceLoop cleanG s 0 `step` intoMe cleanG s
        in coverGraph fullMap

main = print ans2
