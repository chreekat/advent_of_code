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
{-# LANGUAGE MultiWayIf #-}

import Tropes
import TwoD
import qualified Data.Array as Array
import qualified Data.Set as Set
import Data.List

ex1, ex2, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day17-ex1.txt")
{-# NOINLINE ex2 #-}
ex2 = unsafePerformIO (readFile "day17-ex2.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day17.txt")

ans1 :: _
ans1 = ex1

ans2 :: _
ans2 = undefined

data Prev = Self | North | South | West | East deriving (Eq, Show)

data P a = M | P a
    deriving (Eq, Show)

instance Ord a => Ord (P a) where
    compare M M = EQ
    compare M _ = GT
    compare _ M = LT
    compare (P a) (P b) = compare a b

instance Num a => Num (P a) where
    (+) M M = M
    (+) M _ = M
    (+) _ M = M
    (+) (P a) (P b) = P (a + b)
    negate M = M
    negate (P a) = P (negate a)
    abs M = M
    abs (P a) = P (abs a)
    signum M = M
    signum (P a) = P (signum a)
    fromInteger = P . fromInteger
    (*) M M = M
    (*) M _ = M
    (*) _ M = M
    (*) (P a) (P b) = P (a + b)

ptWt grid = _1of3 . (grid Array.!)
ptPath grid = _2of3 . (grid Array.!)
ptPrev grid = _3of3 . (grid Array.!)

iPrev grid i@(x,y) = case ptPrev grid i of
    Self -> Nothing
    North   -> Just (x -1, y)
    South   -> Just (x + 1, y)
    West    -> Just (x, y - 1)
    East    -> Just (x, y + 1)

backtrace grid i = grid Array.! i : case iPrev grid i of
    Nothing -> []
    Just j  -> backtrace grid j

opposite North = South
opposite South = North
opposite West = East
opposite East = West

same [] = True
same (x:xs) = all (== x) xs

allowedPrev grid dst i =
    let prev = ptPrev grid i
        shortBacktrace = fmap _3of3 $ take 3 $ backtrace grid i
        dstPrev = computePrev dst i
    in if | prev == Self -> True
          | opposite prev == dstPrev -> False
          | length shortBacktrace < 3 -> True
          | same (dstPrev: shortBacktrace) -> False
          | otherwise -> True

computePrev (x1,y1) (x0,y0) = case (x1 - x0, y1 - y0) of
    (1, 0) -> North
    (-1, 0) -> South
    (0, 1) -> West
    (0, -1) -> East
    _ -> Self

startPoint arr i = (i, (read [arr Array.! i], P 0, Self))

emptyPaths = fmap (\wt -> (read [wt], M, Self))

startPath :: TwoD Char -> (Int, Int) -> TwoD (Int, P Int, Prev)
startPath arr i = emptyPaths arr Array.// [startPoint arr i]


stepPath :: TwoD (Int, P Int, Prev) -> [(Int, Int)] -> TwoD (Int, P Int, Prev)
stepPath grid [] = grid
stepPath grid (i:is) =
    let neighs = filter (allowedPrev grid i) $ neighbors grid i
        prev = case neighs of [] -> Nothing
                              xs -> Just $ minimumBy (compare `on` ptPath grid) xs
        newCell = do
            prev' <- prev
            let newPath = ptPath grid prev' + P (ptWt grid i)
            if newPath < ptPath grid i
                then Just (ptWt grid i, newPath, computePrev i prev')
                else Nothing
        
        newGrid = maybe grid (\newCell -> grid Array.// [(i, newCell)]) newCell
    in case newCell of
        Nothing -> stepPath grid is
        Just newCell -> stepPath newGrid (Set.toList $ Set.fromList (neighs ++ is))

--main = putStr $ showTwoD'' showPath $ stepPath (startPath (twoD $ ex1) (0,0)) (Array.indices $ twoD $ ex1)
main = let g = stepPath (startPath (twoD $ ex1) (0,0)) (Array.indices $ twoD $ ex1)
           path = markPath g (snd (Array.bounds g))
        in putStr $ showTwoD $ honk (twoD ex1) path
    --in print $ ptPath g (snd (Array.bounds g))

showPrev :: Prev -> Char
showPrev Self = 'o'
showPrev North = 'v'
showPrev South = '^'
showPrev West = '>'
showPrev East = '<'

showPath (x, _, Self) = show x
showPath (_, _, p) = [showPrev p]

markPath grid i = case iPrev grid i of
    Nothing -> [(i, showPrev $ ptPrev grid i)]
    Just j -> (i, showPrev $ ptPrev grid i) : markPath grid j

honk grid xs = grid Array.// xs
