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
import Tropes

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day4-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day4.txt")

ans1 :: _
ans1 = dat

ans2 :: _
ans2 = dat

main1 = print $ length $ catMaybes $ allDirs $ TwoD.twoD ans1
main = print $ 
    let aa = sort $ catMaybes $ allDirs2 $ TwoD.twoD ans2
        x = length aa
        y = length $ List.nub aa
    in x - y

-- 2 * n + c = x
--     n + c = y
--     c = y - n
-- 2 * n + y - n = x
-- n + y = x
-- n = x - y
allDirs a = concatMap (dirs a) (Array.indices a)
allDirs2 a = concatMap (dirs2 a) (Array.indices a)

dirs a (x,y) = [ dir' (i,j) (x,y) a| i <- [-1..1], j <- [-1..1], i /= 0 || j /= 0 ]
dirs2 a (x,y) = [ dir'' (i,j) (x,y) a| i <- [-1..1], j <- [-1..1], i /= 0 && j /= 0 ]

dir' a b = dir a b 'X'
dir'' a b = dir2 a b 'M'

dir (i',j') (i,j) l arr
    | Just 'X' <- Array.lookup (i,j) arr
    , Just 'M' <- Array.lookup (y,x) arr
    , l == 'X'
    = nxt 'M'
    | Just 'M' <- Array.lookup (i,j) arr
    , Just 'A' <- Array.lookup (y,x) arr
    , l == 'M'
    = nxt 'A'
    | Just 'A' <- Array.lookup (i,j) arr
    , Just 'S' <- Array.lookup (y,x) arr
    , l == 'A'
    = Just (i-2*i',j-2*j')
    | otherwise = Nothing
    where
        (y,x) = (i+i',j+j')
        nxt m = dir (i',j') (y,x) m arr

dir2 (i',j') (i,j) l arr
    | Just 'M' <- Array.lookup (i,j) arr
    , Just 'A' <- Array.lookup (y,x) arr
    , l == 'M'
    = nxt 'A'
    | Just 'A' <- Array.lookup (i,j) arr
    , Just 'S' <- Array.lookup (y,x) arr
    , l == 'A'
    = Just (i,j)
    | otherwise = Nothing
    where
        (y,x) = (i+i',j+j')
        nxt m = dir2 (i',j') (y,x) m arr
