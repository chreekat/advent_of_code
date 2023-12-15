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
{-# HLINT ignore "Use list comprehension" #-}

import Tropes
import Combinatorics
import qualified Data.Map as Map
import Data.List

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day12-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day12.txt")

-- With a space of length 'space', and a list of objects with
-- lengths objs, the number of ways to fit the objects in the space with at
-- least one gap between them is:
options space objs =
    let space' = space - sum objs + 1
    in choose space' (length objs)

options' f space objs = map (map f) $ options space objs

-- Given a damaged map and list of objects that should fit on the map, get the
-- options that would fit the map.
damagedMapOptions dammap objs =
    let allOptions = map (rehydrate objs) $ options (length dammap) objs
    in allOptions

rehydrate = go where
    go (o:os) (True:ms) = o : go os ms
    go os (False:ms) = 0 : go os ms
    go _ [] = []

showMap [m] = replicate m '#'
showMap (0:ms) = '.' : showMap ms
showMap (m:ms) = replicate m '#' <> "." <> showMap ms
showMap [] = []

suitable :: String -> String -> Bool
suitable origMap testMap = (and $ zipWith f origMap testMap) && ((>=) `on` length) origMap testMap
    where
        f '#' '#' = True
        f '.' '.' = True
        f '?' _ = True
        f _ _ = False

pRow s =
    let [mmap, nums] = splitOn " " s
    in (mmap, map read $ splitOn "," nums)

input = map pRow . lines $ dat

doRow (dammap, objs) = filter (suitable dammap) $ map showMap $ damagedMapOptions dammap objs

ans1 :: _
ans1 = print $ sum $ map (length . doRow) input

unfoldMap = intercalate "?" . replicate 5
unfoldRow (mmap, objs) = (unfoldMap mmap, concat $ replicate 5 objs)

type Key = (String, [Int])

-- Given the original map, we can construct a Map of subsolutions. The index
-- into the subsolution is (submap, subset of objects).
solns :: String -> [Int] -> Map.Map Key Int
solns mmap objs =
    let subsolns = Map.fromList $ map (uncurry (solutionsForThisKey subsolns)) $ allKeys mmap objs
    in subsolns

-- Get a solution to a key by building the Map of subsolutions and then indexing
-- it.
soln k = uncurry solns k Map.! k

-- All the interesting keys into a solution map given an initial map and list of
-- objs is overprescribed like so, but that's ok. Only the ones we need are
-- evaluated.
allKeys mmap objs = [ (m, o) | m <- tails mmap, o <- tails objs]

-- DSL token for tagging solutions with the key the solve for.
solves :: Int -> k -> (k, Int)
pics `solves` k = (k,pics)

--    if first obj fits at beginning of map, then solutionsForThisKey are solutions for the rest of the objs with a shrunken map with this obj prepended to them.
--    as a special case, if there are no other objs, then solutionsForThisKey are simply this obj with the rest of the map prepended.
--    other solutions for this key include solutions for all objs but a shrunken map with '.' prepended.

-- Here we recursively define the solutions for the given key using the full map
-- of all (sub)solutions. 
solutionsForThisKey :: Map.Map Key Int -> String -> [Int] -> (Key, Int)
-- No objects = if there are no remaining '#', then there's one solution.
-- Otherwise, no solutions.
solutionsForThisKey _ s o@[]
    | notElem '#' s = 1 `solves` (s,o)
    | otherwise      = 0 `solves` (s,o)
-- Empty spaces with objects remaining = no solutions
solutionsForThisKey _ s@[] o@(_:_) = 0 `solves` (s,o)
-- If the first object is suitable at the front of the space, then solutions for
-- this key include all subsolutions for the remaining objects prepended with a
-- '.'. If there are no other objects, we don't need to prepend a '.'.
-- If the first space is a '?', then solutions for this key also include
-- subsolutions for the same objs and the tail of the space, prepended with '.'.
-- Finally, if the front of the space is a '.', then solutions are all
-- subsolutions starting in the next space.
solutionsForThisKey subsolns mmap@(m:ms) objs@(o:os)

    | suitable mmap (showMap [o])
    , [] <- os
    , '#' <- m
    = subsolns Map.! (drop o mmap, os) `solves` (mmap, objs)

    | suitable mmap (showMap [o])
    , [] <- os
    , '?' <- m
    = (subsolns Map.! (drop o mmap, os) 
        + subsolns Map.! (ms, objs)) `solves` (mmap, objs)

    | suitable mmap (showMap [o,0])
    , '#' <- m 
    = subsolns Map.! (drop (o+1) mmap, os) `solves` (mmap, objs)

    | suitable mmap (showMap [o,0])
    , '?' <- m 
    = (subsolns Map.! (drop (o+1) mmap, os)
        + subsolns Map.! (ms, objs)) `solves` (mmap, objs)

    | m /= '#'
    = subsolns Map.! (ms, objs) `solves` (mmap, objs)

    | otherwise = 0 `solves` (mmap, objs)

ans2 :: _
ans2 =
    let is = map unfoldRow input
    in print $ sum $ map soln is

main = ans2
