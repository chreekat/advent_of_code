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
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Tropes
import SimpleParse
import Data.Map qualified as Map

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day1-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day1.txt")

ans1 :: _
ans1 = 
    let [a, b] = map sort $ transpose $ map (map read . words) $ lines dat
    in sum $ zipWith (\x -> abs . (x -)) a b

ans2 :: _
ans2 = 
    let [a, b] :: [[Int]] = map sort $ transpose $ map (map read . words) $ lines dat
        counts = foldr (Map.alter (Just . maybe 1 (+1))) Map.empty b
        total = foldr (\a c -> c + a * fromMaybe 0 (Map.lookup a counts)) 0 a
    in total

main = print ans2
