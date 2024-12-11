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

import Data.Array qualified as Array
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence qualified as Seq
import Data.Vector qualified as Vector
import Tropes

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day2-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day2.txt")

ans1 :: _
ans1 =
    let reports :: [[Int]] = map (map read . words) $ lines dat
    in length $ mapMaybe checkReport reports

checkLevel Nothing _ = Nothing
checkLevel (Just x) (l,r)
    | abs (l - r) `elem` [1..3]
    && (x == 0 || ((==) `on` signum) (l-r) x) = Just (l - r)
    | otherwise = Nothing

checkReport = foldl' checkLevel (Just 0) . (zip <*> tail)

mkSkips :: [Int] -> [[Int]]
mkSkips x = map toList $ map (`Seq.deleteAt` Seq.fromList x) [0..length x - 1]

checkReport2 = Maybe.listToMaybe . Maybe.mapMaybe checkReport . mkSkips

ans2 :: _
ans2 =
    let reports :: [[Int]] = map (map read . words) $ lines dat
    in length $ mapMaybe checkReport2 reports

main = print ans2
