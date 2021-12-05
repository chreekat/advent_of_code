module Tropes (pTraceShowId, on, sort, fromMaybe, partition, isJust, isNothing, Last (..), pTraceShow, First (..), pPrint, All (..), intercalate, splitOn, traceShow, traceShowId, foldl', transpose, unsafePerformIO, module Tropes) where

import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import System.IO.Unsafe
import Text.Pretty.Simple

import Debug.Pretty.Simple
import Debug.Trace

sum3 (a, b, c) = sum [a, b, c]

prod2 (a, b) = product [a, b]

_3to2 (a, b, c) = (a, b)

pars = splitOn "\n\n"
unpars = intercalate "\n\n"


range x y = if x > y then reverse [y..x] else [x..y]

mapAlter :: Ord k => (Maybe a -> Maybe a) -> k -> Map.Map k a -> Map.Map k a 
mapAlter = Map.alter

mapEmpty = Map.empty

mapToList = Map.toList

mapFilter = Map.filter

mapSingleton = Map.singleton

mapUnionsWith :: (Foldable f, Ord k) => (a -> a -> a) -> f (Map.Map k a) -> Map.Map k a 
mapUnionsWith = Map.unionsWith
