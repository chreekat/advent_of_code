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
ex1 = unsafePerformIO (readFile "day7-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day7.txt")

ans1 :: _
ans1 = 
    let os =
            [ (tot, ops)
            | p <- lines dat
            , let [x,y] = splitOn ":" p
            , let tot = read x
            , let ops = map read $ words y
            ] :: [(Int, [Int])]
        results = map process os
    in sum $ mapMaybe (fmap _1of3) results

process p@(val, operands) = 
    let actions = replicateM (length operands - 1) [0,1,2]
    in chug val operands actions
   
chug val operands [] = Nothing
chug val operands (a:as) 
    | Just x <- combo val operands a
    , x == val = Just (val, operands, a)
    | otherwise = chug val operands as

traceShowId = id
-- all possible permutations of [(+),(*)] of a given length
-- [+,+], [+,*], [*,+], [*,*]
-- It's [0..2**n-1] where 0 = + and 1 = *


-- count number of base-10 digits in a number without going through show
digits x = f x 0 where
    f 0 acc = acc
    f x acc = f (x `div` 10) (acc + 1)

-- given [a,b] and [0], execute a+b
-- given [a,b,c] and [0,1], execute (a + b) * c
combo tot [] _ = Just tot
combo tot (a:as) acts = f a as acts where
    f acc [] [] = Just acc
    f acc (v:vs) (o:os)
        | acc > tot = Nothing
        | otherwise = f (step acc (v,o)) vs os

-- given a, ('b',0), execute a + b
-- given a, ('b',1), execute a * b
-- given a, ('b',2), execute 'a'<>'b'
step acc (val,0) = {-# SCC "+" #-} acc + val
step acc (val,1) = {-# SCC "*" #-} acc * val
--step acc (val,2) = {-# SCC "||" #-} read $ concatMap show [acc, val]
--step acc (val,2) = {-# SCC "||" #-} acc * 10 ^ (length (show val)) + val
step acc (val,2) = {-# SCC "||" #-} acc * 10 ^ (digits val) + val

-- foldl' f z [] = z
-- foldl' f z (x:xs) = foldl' f (f z x) xs


ans2 :: _
ans2 = undefined

main = print ans1
