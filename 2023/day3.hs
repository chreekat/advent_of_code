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

import Tropes
import qualified Data.Array as Arr

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day3-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day3.txt")

toVec s =
    let w = length (head ls)
        h = length ls
        ls = lines s
        ixs = concatMap (\x -> map (\y -> (x, y)) [0..h-1]) [0..w-1]
    in Arr.array ((0, 0), (w-1, h-1)) (zip ixs (concat ls))

numBoundary (x,y) arr = 
    let
        bds = Arr.bounds arr
        yMin = snd $ last $ takeWhile (\(x',y') -> y' >= 0 && isDigit (arr Arr.! (x',y'))) $ iterate (\(x',y') -> (x',y'-1)) (x,y)
        yMax = snd $ last $ takeWhile (\(x',y') -> Arr.inRange bds (x',y')  && isDigit (arr Arr.! (x',y'))) $ iterate (\(x',y') -> (x',y'+1)) (x,y)
    in (yMin, yMax)

-- All indices surrounding the number prescribed by $numBoundary (x, y) arr$
numNeighbors (x,y) arr =
    let
        (yMin, yMax) = numBoundary (x,y) arr
        b = Arr.bounds arr
    in  filter (\r -> Arr.inRange b r && not (Arr.inRange ((x,yMin),(x,yMax)) r)) (Arr.range ((x-1, yMin-1), (x+1, yMax+1)))

naughtyList = ['0'..'9'] ++ ['.']
isSymbol' = not . (`elem` naughtyList)

isPartNum (x,y) arr = any isSymbol' (map (arr Arr.!) (numNeighbors (x,y) arr))

partNums :: Arr.Array (Int, Int) Char -> [Int]
partNums arr = go (0,0) [] where
    bounds = Arr.bounds arr
    nextI (x,y) = if Arr.inRange bounds (x, y+1) then (x, y+1) else (x+1, 0)
    go i@(x,y) parts
        | not (Arr.inRange bounds i) = parts
        | isDigit (arr Arr.! i) && isPartNum i arr =
            let (yMin, yMax) = numBoundary i arr
            in go (nextI (x, yMax)) (read (map (arr Arr.!) (Arr.range ((x,yMin),(x,yMax)))) : parts)
        | otherwise = go (nextI i) parts

ans1 :: _
ans1 = let
    v = toVec dat
    b = partNums v
    in sum b

gearNeighbors arr (x,y)  =
    let bds = Arr.bounds arr
    in filter (/= (x,y)) $ filter (Arr.inRange bds) (Arr.range ((x-1, y-1), (x+1, y+1)))

maybeNumBoundary arr i =
    (fst i,) <$> if isDigit (arr Arr.! i) then Just (numBoundary i arr) else Nothing

gearNums arr i = nub $ sort $ mapMaybe (maybeNumBoundary arr) (gearNeighbors arr i)

gearRatio arr i
    = case gearNums arr i of
        [(x,yrng), (x', yrng')] -> read [ arr Arr.! (x,y) | y <- Arr.range yrng] * read [ arr Arr.! (x',y') | y' <- Arr.range yrng']
        _ -> 0

isGear = (== '*')

gearSum arr (i,e) | isGear e = gearRatio arr i
                  | otherwise = 0

ans2 :: _
ans2 = let
    v = toVec dat
    in sum $ map (gearSum v) (Arr.assocs v)

-- ghcid needs this?
main = print ans2
