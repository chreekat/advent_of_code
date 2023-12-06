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
import SimpleParse
import Control.Parallel.Strategies

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day6-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day6.txt")

pRaces = do
    lexx (p_string "Time:")
    times <- many (lexx p_int)
    lexx (p_string "Distance:")
    dists <- many (lexx p_int)
    pure (zip times dists)

dist t hold = hold * (t - hold)

winners (t,d) = filter (>d) (withStrategy (parBuffer 100 rseq) $ map (dist t) [1..t-1])

numWinners = length . winners

ans1 :: _
ans1 = product $ map numWinners $ parse' pRaces dat

pRace :: Parser (Int,Int)
pRace = do
    lexx (p_string "Time:")
    times <- many (lexx p_int)
    lexx (p_string "Distance:")
    dists <- many (lexx p_int)
    let time = read $ concatMap show times
        dist = read $ concatMap show dists
    pure (time, dist)


ans2 :: _
ans2 = numWinners  $ parse' pRace dat

main = print ans2
