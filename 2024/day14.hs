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
import SimpleParse qualified as Parse

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day14-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day14.txt")

ans1 :: _
ans1 = 
    let space = (101,103)
        count = 100
        Right robots = Parse.parse (Parse.many parse) mempty dat
        result = sort $ map (\(p,v) -> moveMany space p v count) robots
        quads = quadrants space result
    in product $ map length quads

quadrants (bx,by) robots =
    let halfX = bx `div` 2
        halfY = by `div` 2
    in
        [ [r | r@(x,y) <- robots, x < halfX, y < halfY]
        , [r | r@(x,y) <- robots, x > halfX, y < halfY]
        , [r | r@(x,y) <- robots, x < halfX, y > halfY]
        , [r | r@(x,y) <- robots, x > halfX, y > halfY]]

-- Move a robot within a space. They wrap, so use rem.
move :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
move (bx, by) (x, y) (dx, dy) = (x', y')
  where
    adjustNeg b n = if n < 0 then b + n else n
    x' = adjustNeg bx $ (x + dx) `rem` bx
    y' = adjustNeg by $ (y + dy) `rem` by

-- Move them multiple times.
moveMany :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> (Int,Int)
moveMany b p (dx,dy) c = move b p (dx*c, dy*c)

parse = do
    _ <- Parse.string "p="
    p_x <- Parse.decimal
    _ <- Parse.string ","
    p_y <- Parse.decimal
    _ <- Parse.string "v="
    v_x <- Parse.signedInt
    _ <- Parse.string ","
    v_y <- Parse.signedInt
    pure ((p_x, p_y), (v_x, v_y))

ans2 :: _
ans2 = undefined

main = print ans1
