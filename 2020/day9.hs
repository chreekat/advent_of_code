{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Data.Bifunctor
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import System.IO.Unsafe
import qualified Data.Vector as V

import SimpleParse

test = unsafePerformIO (readFile "day9-test.txt")
input = unsafePerformIO (readFile "day9-input.txt")

main = print (part1test == 127, part2test == 62, part1actual, part2actual)

part1 sz = snd . head . filter (not . fst)  . foo sz
part1test = part1 5 test
part1actual = part1 25 input

foo :: Int -> String -> [(Bool,Int)]
foo sz = validity sz . map read . lines



valid sz nums i n = 
    let v = any id [ a + b == n
                   | let pre = toList (V.slice (i - sz) sz nums)
                   , a <- pre
                   , b <- pre
                   , a /= b ]
    in (v, n)

validity sz nums =
    let (preamble, nums') = splitAt sz nums
        vums = V.fromList nums
        end = V.length vums - 1
    in map (True,) preamble <> zipWith (valid sz vums) [sz..] nums'


encRange goal nums i =
    listToMaybe
        [ rng
        | n <- [1 .. V.length nums - i]
        , let rng = V.slice i n nums
        , V.sum rng == goal
        ]


part2 inp goal = minn + maxx where
    ints :: V.Vector Int
    ints = V.fromList (map read (lines inp))
    rng =
        fromJust
            (getFirst
                (foldMap
                    (First . encRange goal ints)
                    [0 .. V.length ints - 1]))
    maxx = maximum rng
    minn = minimum rng

part2test = part2 test part1test

part2actual = part2 input part1actual
