module Tropes (chunksOf, traceM, pTraceM, pTraceShowM, catMaybes, Map.keysSet, zipWithM, evalState, get, coerce, Max (..), isSymbol, isDigit, isUpper, Set, Sum (..), replicateM, replicateM_, execState, second, runState, (<=<), traverse_, gets, State (..), modify, mapMaybe, NE.NonEmpty (..), nub, toList, first, (Map.!), Map.mapWithKey, join, Comonad (..), Map.keys, fromJust, findIndices, findIndex, comparing, sortBy, (\\), minimumBy, genericLength, Map.fromListWith, Map.mapKeysWith, Map.Map, iterate', group, (&&&), pTraceShowId, on, sort, fromMaybe, partition, intersect, isJust, isNothing, Last (..), pTraceShow, First (..), pPrint, All (..), intercalate, splitOn, traceShow, traceShowId, foldl', transpose, unsafePerformIO, elemIndex, module Tropes, Vec.ifoldl', Vec.slice) where

import Control.Arrow
import Control.Comonad
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer hiding (First, Last)
import Data.Bool
import Data.Char
import Data.Coerce
import Data.Foldable
import Data.Function
import Data.Ord
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid (First (..), Last (..))
import Data.Semigroup hiding (First, Last)
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO.Unsafe
import Text.Pretty.Simple
import qualified Data.Vector as Vec
import Data.Vector (Vector)

import Debug.Pretty.Simple
import Debug.Trace

-- Paterson always used irrefutable patterns for tuples. Why?
_3to2 (a, b, c) = (a, b)

assoc ((a, b), c) = (a, (b, c))

avg :: Fractional a => [a] -> a
avg xs = sum xs / genericLength xs

mapAdjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
mapAdjust = Map.adjust

mapAlter :: Ord k => (Maybe a -> Maybe a) -> k -> Map.Map k a -> Map.Map k a
mapAlter = Map.alter

mapDelete :: Ord k => k -> Map k a -> Map k a
mapDelete = Map.delete

mapEmpty = Map.empty

mapFilter = Map.filter

mapFromList :: Ord k => [(k, a)] -> Map.Map k a
mapFromList = Map.fromList

mapInsert :: Ord k => k -> a -> Map k a -> Map k a
mapInsert = Map.insert

mapLookup :: Ord k => k -> Map k a -> Maybe a
mapLookup = Map.lookup

mapSingleton = Map.singleton

mapSize = Map.size

mapToList = Map.toList

mapUnionsWith :: (Foldable f, Ord k) => (a -> a -> a) -> f (Map.Map k a) -> Map.Map k a
mapUnionsWith = Map.unionsWith

mapUnionWith :: Ord k => (a -> a -> a) -> Map.Map k a -> Map.Map k a -> Map.Map k a
mapUnionWith = Map.unionWith

mapUnions :: Ord k => [Map.Map k a] -> Map.Map k a
mapUnions = Map.unions

median xs =
    let n = length xs
     in sort xs !! max 0 (n `div` 2)

pars = splitOn "\n\n"
prod2 (a, b) = product [a, b]

range x y = if x > y then reverse [y .. x] else [x .. y]

setEmpty = Set.empty

setFromList :: Ord a => [a] -> Set a
setFromList = Set.fromList

setMap :: Ord k => (a -> k) -> Set a -> Set k
setMap = Set.map

setMember :: Ord k => k -> Set k -> Bool
setMember = Set.member

setPartition = Set.partition

setSingleton = Set.singleton

setSize = Set.size
setUnion :: Ord k => Set k -> Set k -> Set k
setUnion = Set.union

sum2 (a, b) = sum [a, b]
sum3 (a, b, c) = sum [a, b, c]

unassoc (a, (b, c)) = ((a, b), c)

unpars = intercalate "\n\n"

mapKeysSet = Map.keysSet

setDelete :: Ord a => a -> Set a -> Set a
setDelete = Set.delete

intersects :: (Foldable f, Eq a) => f [a] -> [a]
intersects = foldr1 intersect

vecUpdate = (Vec.//)
veci = (Vec.!)
vecFromList = Vec.fromList
