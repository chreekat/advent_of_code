{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-unused-top-binds #-}

import Data.Function
import Data.List
import Data.Ratio
import Data.Maybe
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as S
import qualified Data.Map as M

--import Debug.Pretty.Simple

main = do
    part1
    part2

part1 = pure ()
part2 = pure ()

data Asteroid = Asteroid { ax :: Int, ay :: Int }
    deriving (Eq, Ord, Show)

data Chart = Chart { cx :: Int, cy :: Int, cAsteroid :: Set Asteroid }
    deriving (Eq, Show)

readAsteroid :: Int -> Int -> Char -> Maybe Asteroid
readAsteroid y x = \case
    '.' -> Nothing
    '#' -> pure (Asteroid x y)
    c   -> error ("Not a chart char: " ++ [c])

readChart :: String -> Chart
readChart str =
    let ys = zip [0..] (lines str)
        h = length ys
        w = length (snd (head ys))
        xs = concatMap blop ys
        blop (y,s) = zipWith (readAsteroid y) [0..] s
    in Chart h w (S.fromList (catMaybes xs))

data Bearing = R (Ratio Int) | L (Ratio Int) | U | D
    deriving (Eq, Ord, Show)

takeBearings :: Chart -> Asteroid -> Set Bearing
--takeBearings Chart{cAsteroid = ch} a = S.map (\a2 -> let b = takeBearing a a2 in pTraceShow (a2,b) b) (S.delete a ch)
takeBearings Chart{cAsteroid = ch} a = S.map (takeBearing a) (S.delete a ch)

takeBearing :: Asteroid -> Asteroid -> Bearing
takeBearing (Asteroid x0 y0) (Asteroid x1 y1) =
    let bx = x1 - x0
        by = y1 - y0
    in case (compare y1 y0, compare x1 x0) of
        (EQ, EQ) -> error "You can go bear yourself"
        {-
        (YY, XX)
        -}
        (_, LT) -> L (by % bx)
        (_, GT) -> R (by % bx)
        (LT, EQ) -> U
        (GT, EQ) -> D

countDetections :: Chart -> Map Asteroid Int
countDetections c@(Chart{cAsteroid = ch}) = M.fromSet (S.size . takeBearings c) ch

maxDetections :: Chart -> (Asteroid, Int)
maxDetections = maximumBy (compare `on` snd) . M.toList . countDetections

dummy = ".#..#\n.....\n#####\n....#\n...##"
