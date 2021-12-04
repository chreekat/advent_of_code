module Tropes (partition, isJust, isNothing, Last (..), pTraceShow, First (..), pPrint, All (..), intercalate, splitOn, traceShow, traceShowId, foldl', transpose, unsafePerformIO, module Tropes) where

import Data.List
import Data.List.Split
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
