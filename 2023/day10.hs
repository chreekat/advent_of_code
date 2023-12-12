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

import Tropes hiding ((!))
import Data.Array 
import TwoD

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day10-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day10.txt")

data P = P Char (Maybe Int) 
    deriving (Show, Eq)

pc (P c _) = c

pn (P c n) = n

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
start :: Array (Int, Int) P -> (Int, Int)
start = fst . head . filter ((== 'S') . pc . snd) . assocs


update g ptr n = g // [(ptr, P (g `gchar` ptr) (Just n))]

step g [] = g
step g (p:ps) = 
    let neighs = fromMe g p
        minDist = 1 + minimum (mapMaybe (g `gnum`) neighs)
    in step (update g p minDist) (filter (maybe True (>= minDist) . (g `gnum`)) neighs ++ ps)

ans1 :: _
ans1 = let g = twoD' (`P` Nothing) dat
           s = start g
        --in showTwoD (\pp -> maybe '-' (const $ pc pp) (pn pp)) $ update g s 0 `step` intoMe g s
           fullMap = update g s 0 `step` intoMe g s
           furthest = maximum (mapMaybe pn (elems fullMap))
        in furthest
--        in showTwoD (maybe '-' (head . show) . pn) $ update g s 0 `step` intoMe g s

ans2 :: _
ans2 = undefined

main = print ans1
