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

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day11-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day11.txt")

ans1 :: _
ans1 =
    let input = map ((,blinks) . read @Int) $ words dat
        blinks = 75
    in sum $ map aaa input

-- make a map where each key of type (num,count) points to how many stones
-- labeled num exist after blinking count times.
aaa = flip evalState Map.empty . fix f where
    f nxt i = do
        mv <- gets (Map.lookup i)
        case mv of
            Just v -> Tropes.traceShow "-" $ pure v
            Nothing -> Tropes.traceShow "+" $ goVal nxt i
    goVal nxt i = do
        v <- mkVal nxt i
        v <$ modify (Map.insert i v)
    mkVal _ (v,0) = pure 1
    mkVal nxt (0,count) = nxt (1, count-1)
    mkVal nxt i@(num::Int,count)
        | (l,r) <- splitNumber num
        , length l == length r
        = do 
            a <- nxt (read @Int l,count-1)
            b <- nxt (read @Int r,count-1)
            pure $ a + b
        | otherwise = nxt (2024 * num, count -1)

splitNumber :: Int -> (String, String)
splitNumber n =
    let s = show n 
    in splitAt (length s `div` 2) s

ans2 :: _
ans2 = undefined

main = print ans1
