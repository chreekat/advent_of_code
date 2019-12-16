{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-unused-top-binds #-}

import Control.Arrow
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
    chart <- readChart <$> readFile "day10-input"
    let res1 = maxDetections chart
        (Asteroid x y) = makeThemSuffer (takeRangedBearings chart (fst res1)) !! 199
    print res1
    print (x * 100 + y)

data Asteroid = Asteroid { ax :: Int, ay :: Int }
    deriving (Eq, Ord, Show)

type Chart = Set Asteroid

readAsteroid :: Int -> Int -> Char -> Maybe Asteroid
readAsteroid y x = \case
    '.' -> Nothing
    '#' -> pure (Asteroid x y)
    c   -> error ("Not a chart char: " ++ [c])

readChart :: String -> Chart
readChart str =
    let ys = zip [0..] (lines str)
        xs = concatMap blop ys
        blop (y,s) = zipWith (readAsteroid y) [0..] s
    in S.fromList (catMaybes xs)

data Bearing = U | R (Ratio Int) | D |  L (Ratio Int)
    deriving (Eq, Ord, Show)

takeBearings :: Chart -> Asteroid -> Set Bearing
takeBearings ch a = S.map (takeBearing a) (S.delete a ch)

type Range = Int

data RangedBearing = RangedBearing Bearing Range
    deriving (Eq, Show, Ord)

takeRangedBearings :: Chart -> Asteroid -> Map Bearing (Map Range Asteroid)
takeRangedBearings ch a =
    let rangedBearings = map (id &&& takeRangedBearing a) (S.toList (S.delete a ch))
        mkBearingMap (a2, RangedBearing b r) = M.singleton b (M.singleton r a2)
    in M.unionsWith (<>) (map mkBearingMap rangedBearings)

   where

obliterate :: Bearing -> Map Bearing (Map Range Asteroid) -> (Asteroid, Map Bearing (Map Range Asteroid))
obliterate b bmap =
    let targets = bmap M.! b
        (_,targ) = M.elemAt 0 targets
        remainder = M.deleteAt 0 targets
        adjMap = if M.null remainder then M.delete b else M.insert b remainder
    in (targ, adjMap bmap)

makeThemSuffer :: Map Bearing (Map Range Asteroid) -> [Asteroid]
makeThemSuffer = wronk U
  where
    wronk start m
        | M.null m = []
        | otherwise
        = case M.lookupGE start m of
            Nothing -> wronk U m
            Just (b, _) ->
                let (ded, remaining) = obliterate b m
                in ded : case M.lookupGT b remaining of
                    Nothing -> wronk U remaining
                    Just (b', _) -> wronk b' remaining

takeBearing :: Asteroid -> Asteroid -> Bearing
takeBearing a1 a2 = let (RangedBearing b _) = takeRangedBearing a1 a2 in b

takeRangedBearing :: Asteroid -> Asteroid -> RangedBearing
takeRangedBearing (Asteroid x0 y0) (Asteroid x1 y1) =
    let bx = x1 - x0
        by = y1 - y0
    in case (compare y1 y0, compare x1 x0) of
        (EQ, EQ) -> error "You can go bear yourself"
        {-
        (YY, XX)
        -}
        (_, LT) -> RangedBearing (L (by % bx)) (abs bx)
        (_, GT) -> RangedBearing (R (by % bx)) (abs bx)
        (LT, EQ) -> RangedBearing U (abs by)
        (GT, EQ) -> RangedBearing D (abs by)

countDetections :: Chart -> Map Asteroid Int
countDetections ch = M.fromSet (S.size . takeBearings ch) ch

maxDetections :: Chart -> (Asteroid, Int)
maxDetections = maximumBy (compare `on` snd) . M.toList . countDetections

dummy = ".#..#\n.....\n#####\n....#\n...##"
