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

import Tropes
import qualified Data.Array as Arr
import qualified Data.Map as Map
import TwoD

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day14-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day14.txt")

step (loc, nextVal, sum) = \case
    'O' -> (pred loc, pred nextVal, sum + nextVal)
    '#' -> (pred loc, pred loc, sum)
    '.' -> (pred loc, nextVal, sum)

row xs = _3of3 $ foldl' step (length xs, length xs, 0) xs

ans1 :: _
ans1 = sum $ map row $ transpose $ lines dat

down (x, y) = (x+1, y)
right (x, y) = (x, y+1)
up (x, y) = (x-1, y)
left (x, y) = (x, y-1)

rollSliceStep direction (nextSpot, arr) loc = case arr Arr.! loc of
    'O' -> (direction nextSpot, arr Arr.// [(loc, '.'), (nextSpot, 'O')])
    '#' -> (direction loc, arr)
    _   -> (nextSpot, arr)

rollSlice direction arr xs = snd $ foldl' (rollSliceStep direction) (head xs, arr) xs

colRanges arr = [ Arr.range ((0, y),(xmax, y)) | y <- [0..ymax] ]
    where
        (_, (xmax, ymax)) = Arr.bounds arr

rowRanges arr = [ Arr.range ((x, 0),(x, ymax)) | x <- [0..xmax] ]
    where
        (_, (xmax, ymax)) = Arr.bounds arr

rollNorth arr =
    let ranges = colRanges arr
    in foldl' (rollSlice down) arr ranges

rollSouth arr =
    let ranges = map reverse $ colRanges arr
    in foldl' (rollSlice up) arr ranges

rollWest arr =
    let ranges = rowRanges arr
    in foldl' (rollSlice right) arr ranges

rollEast arr =
    let ranges = map reverse $ rowRanges arr
    in foldl' (rollSlice left) arr ranges

spinCycle = rollEast . rollSouth . rollWest . rollNorth

findCycle (count, countMap, indexMap) x =
    case Map.lookup x countMap of
        Just n -> (n, count, indexMap)
        Nothing ->
            findCycle
                (succ count
                , Map.insert x count countMap
                , Map.insert count x indexMap)
                (spinCycle x)

northLoad arr i@(x,_) =
    let xmax = fst $ snd $ Arr.bounds arr
    in if arr Arr.! i == 'O' then xmax + 1 - x else 0


totalNorthLoad arr = sum $ map (northLoad arr) $ Arr.indices arr

-- Translating i
-- x = [0,1,2,3,4,5,6,7,8,9,10,11,12,13]
--        ^         ^          
--        i         n
--     [0,1,2,3,4,5,1,2,3,4, 5, 1, 2, 3]
-- At count n, we found i in the countMap. That means 6 = 1, 7 = 2, 8 = 3, 9 =
-- 4, 10 = 5, 11 = 1, and so on. 
--
ans i n k = (k - i) `mod` (n - i) + i

ans2 :: _
ans2 =
    let array = twoD dat
        (initSpan, cycleEnd, iterMap) = findCycle (0, Map.empty, Map.empty) array
    in [ totalNorthLoad $ iterMap Map.! ans initSpan cycleEnd 1000000000 ]

main = pPrint ans2
