module Tropes ((<|>), chunksOf, trace, traceM, pTraceM, pTraceShowM, catMaybes, Map.keysSet, zipWithM, evalState, get, coerce, Max (..), isAlpha, isSymbol, isDigit, isUpper, Set, Sum (..), replicateM, replicateM_, execState, second, runState, (<=<), traverse_, gets, modify, mapMaybe, NE.NonEmpty (..), nub, toList, first, (Map.!), Map.mapWithKey, join, Comonad (..), Map.keys, fromJust, findIndices, findIndex, comparing, sortBy, (\\), minimumBy, genericLength, Map.fromListWith, Map.mapKeysWith, Map.Map, iterate', group, (***), (&&&), pTraceShowId, on, sort, fromMaybe, partition, intersect, isJust, isNothing, Last (..), pTraceShow, First (..), pPrint, All (..), intercalate, splitOn, traceShow, traceShowId, foldl', transpose, unsafePerformIO, elemIndex, module Tropes, Vec.ifoldl', Vec.slice, fix) where

import Control.Applicative
import Control.Arrow
import Control.Comonad
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Coerce
import Data.Foldable
import Data.Function
import Data.Ord
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid (First (..), Last (..))
import Data.Semigroup hiding (First, Last)
import Data.Set (Set)
import System.IO.Unsafe
import Text.Pretty.Simple
import qualified Data.Vector as Vec

import Debug.Pretty.Simple
import Debug.Trace
import Data.Bits

-- Paterson always used irrefutable patterns for tuples. Why?
_3to2 (a, b, c) = (a, b)
_1of3 (a, _, _) = a
_2of3 (_, b, _) = b
_3of3 (_, _, c) = c

_1of4 (a, _, _, _) = a
_2of4 (_, b, _, _) = b
_3of4 (_, _, c, _) = c
_4of4 (_, _, _, d) = d

-- @toBits id [True, True, False] = 6@
toBits :: Bits bits => (a -> Bool) -> [a] -> bits
toBits f = foldr (\(x, a) bits -> if f a then setBit bits x else bits) zeroBits . reverse . zip [0 ..] . reverse

fromBits :: FiniteBits bits => (Bool -> a) -> Int -> bits -> [a]
fromBits toA len b = pushBit (0, [])
    where
        pushBit (n, as) | n < len = pushBit (succ n, toA (testBit b n) : as)
                        | otherwise = as

hammingDistance :: Bits b => b -> b -> Int
hammingDistance x = popCount . xor x

assoc ((a, b), c) = (a, (b, c))

avg :: Fractional a => [a] -> a
avg xs = sum xs / genericLength xs

median xs =
    let n = length xs
     in sort xs !! max 0 (n `div` 2)

pars = splitOn "\n\n"
prod2 (a, b) = product [a, b]

range x y = if x > y then reverse [y .. x] else [x .. y]

sum2 (a, b) = sum [a, b]
sum3 (a, b, c) = sum [a, b, c]

unassoc (a, (b, c)) = ((a, b), c)

unpars = intercalate "\n\n"

intersects :: (Foldable f, Eq a) => f [a] -> [a]
intersects = foldr1 intersect

digit c = ord c - ord '0'
