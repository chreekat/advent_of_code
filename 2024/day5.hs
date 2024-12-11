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
import Set qualified
import Tropes
import TwoD qualified

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day5-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day5.txt")

ans1 :: _
ans1 = dat

ans2 :: _
ans2 = dat

main1 = 
    let [ss, zz] = pars ans1
        (ups, set) = (map (map read . splitOn ",") (lines zz), Set.fromList (map o (lines ss)))
        ok = mapMaybe (good set) ups
    --in print $ map (check set) ups
    in print $ sum $ map (\l -> l !! (length l `div` 2)) ok

main = 
    let [ss, zz] = pars ans2
        (ups, set) = (map (map read . splitOn ",") (lines zz), Set.fromList (map o (lines ss)))
        notok = mapMaybe (bad set) ups
    --in print $ map (check set) ups
    in print $ sum $ map (\l -> l !! (length l `div` 2)) (map (fix set) notok)

-- "xx|yy" -> (xx,yy)
o :: String -> (Int,Int)
o ss = let [x,y] = splitOn "|" ss in (read x,read y)

good set orig = maybe (pure orig) (const Nothing) (checkUpdate set orig)

bad set orig = orig <$ checkUpdate set orig

checkUpdate _set [] = Nothing
checkUpdate set orig@(x:xs) = check1 set x xs <|> checkUpdate set xs

check1 _set _ [] = Nothing
check1 set x (y:ys) = Set.lookup (y,x) set <|> check1 set x ys

fix set xs = 
    let c x y = if Set.member (x,y) set then LT else GT
    in List.sortBy c xs
