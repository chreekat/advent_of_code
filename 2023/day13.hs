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

splits xs = zip (map reverse (tail $ init $ inits xs)) (tail $ init $ tails xs)

reflectionPointP x (ls, rs) = x == (sum $ zipWith hammingDistance ls rs)

--reflectionPoints :: Eq a => [a] -> [Bool]
reflectionPoints x = map (reflectionPointP x) . splits 

--reflectionPoint :: Eq a => [a] -> Maybe Int
reflectionPoint x = foldr (\x rs -> if x then Just 1 else fmap (1 +) rs) Nothing . reflectionPoints x

score s | Just r <- reflectionPoint 0 (rowHashes s) = 100 * r
        | Just c <- reflectionPoint 0 (columnHashes s) = c

{- reflectionPoint  

i is a reflection point if elements (i-1, i-2, ...) equals elements (i,i+1,...)
computing reflPoint for elements 1, ... n-1 smells like a zip of tails and heads

-}
ans1 :: _
ans1 = let f = sum $ map score (pars ex1)
    in f
    --in tail $ init $ tails xs

smudgedReflectionPointP l r = 1 == hammingDistance l r

-- smudgedSplits = map smudgedReflectionPointP . splits

toA True = '#'
toA False = '.'

score2 s | Just r <- reflectionPoint 1 (rowHashes s) = 100 * r
         | Just c <- reflectionPoint 1 (columnHashes s) = c

ans2 :: _
ans2 = let f = sum $ map score2 (pars dat)
    in f

main = print ans2
