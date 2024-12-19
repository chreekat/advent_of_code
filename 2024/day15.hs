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
ex1 = unsafePerformIO (readFile "day15-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day15.txt")

ans1 :: _
ans1 =
    let [map_, moves] = pars dat
        Just me = Array.find (== '@') m
        m = TwoD.twoD map_
        final = snd $ List.foldl' move (me,m) $ concat $ lines moves -- TwoD.showTwoD (snd (List.foldl' move moves (me,m)))
        gps ((x,y),c) = if c == 'O' then 100 * x + y else 0
    in show $ sum $ map gps $ Array.assocs final

move (i,m) d = 
    let ups = updates dir i m
        Just newI = dir i m
        dir = case d of
            '>' -> TwoD.east
            '<' -> TwoD.west
            '^' -> TwoD.north
            'v' -> TwoD.south
            _ -> error "bad direction"
    in case ups of
        Nothing -> (i,m)
        Just ups' -> (newI, m Array.// ((i,'.') : ups'))

updates dir i m =
    let cell = m Array.! i
        newI = dir i m
    in case cell of 
        '#' -> Nothing
        '.' -> pure []
        _ -> case newI of
            Nothing -> Nothing
            Just i' -> fmap ((i', cell):) (updates dir i' m)

ans2 :: _
ans2 = undefined

main = putStrLn ans1
