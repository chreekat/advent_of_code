{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

import Control.Comonad.Store
import Control.DeepSeq
import Control.Exception
import Data.Bifunctor
import Data.Bool
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import GHC.Generics
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import System.IO.Unsafe
import Text.Pretty.Simple
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

import Ary
import SimpleParse

import Debug.Pretty.Simple

inp = pGrid . unsafePerformIO . readFile
test = inp "day11-test.txt"
input = inp "day11-input.txt"

main = do
    print part1test
    print part1actual
    print part2test
    print part2actual

part1test = part1 test
part1actual = part1 input
part2test = part2 test
part2actual = part2 input

data Cel = Ey | Oc | Fl
    deriving (Eq, Show, Generic, NFData)

rCel '#' = Just Oc
rCel 'L' = Just Ey
rCel '.' = Just Fl
rCel _ = Nothing

printCel Oc = "#"
printCel Ey = "L"
printCel Fl = "."

neighbors ix@(i,j) = [ (i', j') | i' <- [i-1 .. i+1], j' <- [j-1 .. j+1], ix /= (i', j')]

crowded a = (>= 4) . length . filter (occupied a) . neighbors

alone a = (== 0) . length . filter (occupied a) . neighbors

occupied a i = Just Oc == (a !? i)

step crowded_ _ a i Oc = bool Nothing (Just (i,Ey)) (crowded_ a i)
step _ alone_ a i Ey = bool Nothing (Just (i,Oc)) (alone_ a i)
step _ _ _ _ Fl = Nothing

pGrid :: String -> Ary (Int,Int) [Int] Cel
pGrid s = 
    let xs = lines s
        xmax = length xs
        ymax = length (head xs)
        cels = mapMaybe rCel s

    in Ary [xmax, ymax] (Map.fromList (zip (indices xmax ymax) cels))

changes step_ a@(Ary _ m) = Map.foldMapWithKey f m where
    f i v = maybeToList (step_ a i v)

stepGrid step_ a@(Ary b m) = Ary b (Map.union (Map.fromList (changes step_ a)) m)

settle step_ a@(Ary b m) = go (changes step_ a) where
    go [] = a
    go cs = settle step_ (Ary b (Map.union (Map.fromList cs) m))

part1 s = 
    let Ary _ res = settle (step crowded alone) s
    in Map.size (Map.filter (== Oc) res)

diagonals :: Ary (Int,Int) [Int] a -> (Int,Int) -> [[(Int,Int)]]
diagonals (Ary [xmax, ymax] _) (x, y)=
    let preX = reverse [1 .. x - 1]
        postX = [x + 1 .. xmax]
        preY = reverse [1 .. y - 1]
        postY = [y + 1 .. ymax]
        yy = repeat y
        xx = repeat x
    in [ zip preX preY -- NW
       , zip preX yy -- N
       , zip preX postY -- NE
       , zip xx postY -- E
       , zip postX postY -- SE
       , zip postX yy -- S
       , zip postX preY -- SW
       , zip xx preY -- W
       ]

occupied2 _ [] = False
occupied2 a (d : ds) =
    case a !? d of
        Just Oc -> True
        Just Ey -> False
        Just Fl -> occupied2 a ds
        Nothing -> error ("no: " <> show d)

crowded2 a = (>= 5) . length . filter (occupied2 a) . diagonals a

alone2 a = (== 0) . length . filter (occupied2 a) . diagonals a

step2 = step crowded2 alone2

part2 s = 
    let Ary _ res = settle step2 s
    in Map.size (Map.filter (== Oc) res)

printAry = putStr . render2Ary printCel
