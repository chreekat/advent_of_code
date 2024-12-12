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
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Array qualified
import Combinatorics qualified as Comb
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
ex1 = unsafePerformIO (readFile "day8-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day8.txt")

ans1 :: _
ans1 = 
    let ary = TwoD.twoD' f dat
        rs = mapMaybe sequence $ Array.assocs ary
        zs = concatMap (concatMap (g (Array.bounds ary)) . Comb.tuples 2 . map fst) $ List.groupBy ((==) `on` snd) $ List.sortOn snd rs
    in length $ nub $ sort $ zs

f '.' = Nothing
f c = Just c

g' lim [(a,b), (c,d)] =
    let x = c - a; y = d - b
    in [(a-x, b-y), (c+x, d+y)]

g lim orig@[(a,b), (c,d)] =
    let x = c - a; y = d - b
        back = takeWhile (Array.inRange lim) [(a-n*x, b-n*y) | n <- [1..]]
        front = takeWhile (Array.inRange lim) [(a+n*x, b+n*y) | n <- [1..]]
    in orig <> back <> front
ans2 :: _
ans2 = undefined

main = print ans1
