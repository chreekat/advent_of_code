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
import Data.Hashable qualified as Hash
import Debug.Trace qualified as Trace

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
        step m = map (\(p,v) -> moveMany space p v m)
    --in findLoop space (step start robots)
    in prettyState start space $ step start robots
    --in product $ map length $ quadrants space (step 50 (step 25 (step 25 robots)))

type State = [((Int, Int), (Int, Int))]

-- fold over positions in a state on the x axis
-- mirror (5,5) [((4,0),v)] = [((0,0),v)]
-- mirror (5,5) [((3,1),v)] = [((1,1),v)]
mirror :: (Int, Int) -> State -> State
mirror (bx,_) = map g where
    g ((x,y),v) = ((bx-x-1,y),v)

start = 6243
jump  = 1
-- start = 16646
-- jump  = 20806

findLoop space = fix f Set.empty start where
    step = map (\(p,v) -> moveMany space p v jump)
    f nxt saved count state =
        let [nw,ne,sw,se] = quadrants space state
            h = Hash.hash state
        --in if nw == mirror space ne && sw == mirror space se
        --    then (count, state)
        --    else if count > 20807 then (0, state) else nxt saved (succ count) (step state)
        in Trace.trace (prettyState count space state) $
            if Set.member h saved
            then start + jump * Set.size saved
            else
                nxt (Set.insert h saved) ( count + jump) (step state)

prettyState c (bx,by) robots = 
    let ps = map fst robots
        draw i
            | i `elem` ps = '#'
            | otherwise = '.'
    in unlines (("HI! " <> show c) : [ [draw (x,y) | x <- [0..bx-1]] | y <- [0..by-1]])

quadrants (bx,by) robots =
    let halfX = bx `div` 2
        halfY = by `div` 2
    in
        [ [r | r@((x,y),_) <- robots, x < halfX, y < halfY]
        , [r | r@((x,y),_) <- robots, x > halfX, y < halfY]
        , [r | r@((x,y),_) <- robots, x < halfX, y > halfY]
        , [r | r@((x,y),_) <- robots, x > halfX, y > halfY]]

-- Move a robot within a space. They wrap, so use rem.
move :: (Int, Int) -> (Int, Int) -> (Int, Int) -> _
move (bx, by) (x, y) (dx, dy) = ((x',y'), (dx,dy))
  where
    adjustNeg b n = if n < 0 then b + n else n
    x' = adjustNeg bx $ (x + dx) `rem` bx
    y' = adjustNeg by $ (y + dy) `rem` by

-- Move them multiple times.
moveMany :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> _
moveMany b p v@(dx,dy) c = 
    let (newP,_) = move b p (dx*c, dy*c)
    in (newP,v)

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

main = putStr ans1
