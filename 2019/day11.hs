{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-unused-top-binds #-}

-- import Control.Arrow
-- import Data.Function
-- import Data.List
-- import Data.Ratio
import Data.Maybe
-- import Data.Set (Set)
import Data.Map (Map)
-- import qualified Data.Set as S
import qualified Data.Map as M

import Intcode

type S = String
type C = Char
type I = Int

main = do
    inp <- readProgramFile "day11-input"
    part1 inp
    part2 inp

data Color = B | W
    deriving (Eq, Show, Ord)

data Loc = Loc I I
    deriving (Eq, Show, Ord)

data Dir = U | R | D | L
    deriving (Eq, Show, Ord, Enum)

data Rot = CW | CCW
    deriving (Eq, Show, Ord)

data Vdir = Vdir Loc Dir
    deriving (Eq, Show, Ord)

type Painting = Map Loc Color

data Status = Status Vdir Painting
    deriving (Eq, Show, Ord)

status0 = Status (Vdir (Loc 0 0) U) M.empty

type Cmd = (Color, Rot)

rotate :: Dir -> Rot -> Dir
rotate L CW = U
rotate d CW = succ d
rotate U CCW = L
rotate d CCW = pred d

readRotate 0 = CCW
readRotate 1 = CW
readRotate _ = error "Error in readRotate"

readColor 0 = B
readColor 1 = W
readColor _ = error "Error in readColor"

move1 :: Vdir -> Vdir
move1 (Vdir (Loc x y) dir) =
    let
        loc = case dir of
            L -> Loc (pred x) y
            D -> Loc x (succ y)
            R -> Loc (succ x) y
            U -> Loc x (pred y)
    in Vdir loc dir

crank :: Cmd -> Status -> (Color, Status)
crank (col, rot) (Status (Vdir loc dir) paint) =
    let
        newVdir@(Vdir l _) = move1 (Vdir loc (rotate dir rot))
        newPaint = M.insert loc col paint
    in (senseColor' l newPaint, Status newVdir newPaint)

senseColor l (Status _ p) = fromMaybe B (M.lookup l p)
senseColor' l = fromMaybe B . M.lookup l

intInput :: Color -> Int
intInput B = 0
intInput W = 1

painter :: Program -> Color -> Painting
painter prog start =
    let intInputs = map (intInput . fst) painterOutputs
        intOutputs = runProgram prog intInputs 
        painterInputs = zip intOutputs (dup painterOutputs)
        painterOutputs = (start, status0) : runPainter painterInputs
        runPainter ((i0,_) : (i1,(_, stat)) : rest)
            = crank (readColor i0, readRotate i1) stat : runPainter rest
        runPainter [] = []
        runPainter [_] = error "Error in runPainter"

        (Status _ paint) = snd (last painterOutputs)
    in paint


minLoc (Loc x0 y0) (Loc x1 y1) = Loc (min x0 x1) (min y0 y1)

minimumLoc1 :: Foldable t => t Loc -> Loc
minimumLoc1 = foldr1 minLoc

maxLoc (Loc x0 y0) (Loc x1 y1) = Loc (max x0 x1) (max y0 y1)

maximumLoc1 :: Foldable t => t Loc -> Loc
maximumLoc1 = foldr1 maxLoc

draw :: Painting -> String
draw p =
    let
        (Loc minx miny) = minimumLoc1 (M.keys p)
        (Loc maxx maxy) = maximumLoc1 (M.keys p)
        paint B = ' '
        paint W = '#'
    in
        unlines $ for [miny .. maxy] $ \y ->
            for [minx .. maxx] $ \x ->
                paint (senseColor' (Loc x y) p)


for = flip map

dup (x:xs) = x : x : dup xs
dup [] = []

part1 prog = print $ M.size (painter prog B)

part2 prog = putStrLn (draw (painter prog W))
