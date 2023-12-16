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
import Data.List
import qualified Data.Map as Map
import Data.Bits

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day13-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day13.txt")

hash :: _ -> Int
hash = toBits (== '#')

rowHashes = map hash . lines

columnHashes = map hash . transpose . lines

splits xs = tail $ init $ zip (map reverse (inits xs)) (tails xs)

reflectionPointP x (ls, rs) = x == (sum $ zipWith hammingDistance ls rs)

reflectionPoints x = map (reflectionPointP x) . splits 

reflectionPointDepth x = foldr (\x rs -> if x then Just 1 else fmap (1 +) rs) Nothing . reflectionPoints x

score s | Just r <- reflectionPointDepth 0 (rowHashes s) = 100 * r
        | Just c <- reflectionPointDepth 0 (columnHashes s) = c

ans1 :: _
ans1 = let f = sum $ map score (pars ex1)
    in f

toA True = '#'
toA False = '.'

score2 s | Just r <- reflectionPointDepth 1 (rowHashes s) = 100 * r
         | Just c <- reflectionPointDepth 1 (columnHashes s) = c

ans2 :: _
ans2 = let f = sum $ map score2 (pars dat)
    in f

main = print ans2
