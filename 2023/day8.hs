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

import Tropes

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day8-ex1.txt")
{-# NOINLINE ex2 #-}
ex2 = unsafePerformIO (readFile "day8-ex2.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day8.txt")

pDat x =
    let [p1,p2] = splitOn "\n\n" x
        pEntry z =
            let [a, _, b, c] = map (filter isAlpha) $ words z
            in (a, (b,c))
    in (head $ lines p1, mapFromList $ map pEntry $ lines p2)

turn n mmap (i:is) node
    | node == "ZZZ" = n
    | otherwise = traceShow node $ case i of
        'L' -> turn (n+1) mmap is (fst (mmap ! node))
        'R' -> turn (n+1) mmap is (snd (mmap ! node))

ans1 :: _
ans1 =
    let
        (instrs, mmap) = pDat dat
    in turn 0 mmap (cycle instrs) "AAA"

cNodes c = setFromList . filter (\[_,_,c'] -> c' == c) . keys

aNodes = cNodes 'A'
zNodes = cNodes 'Z'

turn2 n mmap (i:is) nodes
    | nodes == zNodes mmap = n
    | otherwise = case i of
        'L' -> turn2 (n+1) mmap is (setMap (fst . (mmap !)) nodes)
        'R' -> turn2 (n+1) mmap is (setMap (snd . (mmap !)) nodes)

turn3 mmap ('L':is) node = node : turn3 mmap is (fst (mmap ! node))
turn3 mmap ('R':is) node = node : turn3 mmap is (snd (mmap ! node))

turn3Outer mmap is aNodes =
    let paths = map (turn3 mmap is) aNodes
        results = map (all (\[_,_,c'] -> c' == 'Z')) paths
        ans = traceShowId $ zip [0..] results
    in traceShow aNodes $ fst $ head $ dropWhile (not . snd) ans


ans2 :: _
ans2 =
    let
        (instrs, mmap) = pDat dat
        turn4 = filter snd . zip [0..] . map (\[_,_,c'] -> c' == 'Z') . turn3 mmap (cycle instrs)
        winners = map (fst . head . turn4)  (toList $ aNodes mmap)
    in foldr1 lcm winners
    --in turn3Outer mmap (cycle instrs) (toList $ aNodes mmap)

main = print ans2
