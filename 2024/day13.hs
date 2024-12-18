{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant map" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use <$>" #-}
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
import Control.Monad.Writer.CPS as Writer
import Data.Foldable qualified as Fold
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import HMap qualified
import Map qualified
import Seq qualified
import Tropes hiding (traceShow, traceShowId)
import Tropes qualified
import TwoD qualified
import Data.List.Split qualified as Split

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day13-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day13.txt")

ans1 :: _
ans1 =
    let extract xs = [xs!!2,xs!!3,xs!!6,xs!!7,xs!!9,xs!!10]
        inputs = map (map (read . drop 2) . extract . Split.split (Split.dropDelims $ Split.dropBlanks $ Split.oneOf " ,\n")) $ pars dat
        run [a_x,a_y,b_x,b_y,_X,_Y] = check (10000000000000 + _X) (10000000000000 + _Y) a_x a_y b_x b_y
    --in map run inputs
    in sum $ map (\(a,b) -> a*3 + b) $ mapMaybe run inputs

ans2 :: _
ans2 = undefined

main = print ans1

check _X _Y a_x a_y b_x b_y =
    let (_A,_B) = (round *** round) $ solve _X _Y a_x a_y b_x b_y
        i = round
        (dx,dy) = dist _A _B (i a_x) (i a_y) (i b_x) (i b_y)
    in if round _X == dx && round _Y == dy
        then Just (_A,_B)
        else Nothing

solve :: Double -> _
solve _X _Y a_x a_y b_x b_y =
    let
        _A = (_X - _B * b_x) / a_x
        _B = (_Y * a_x - _X * a_y) / (a_x * b_y - a_y * b_x)
    in (_A, _B)


dist :: Int -> _
dist _A _B a_x a_y b_x b_y
    = (_A * a_x + _B * b_x, _A * a_y + _B * b_y)
