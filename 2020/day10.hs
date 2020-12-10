{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import System.IO.Unsafe
import qualified Data.Vector as V

import SimpleParse

inp :: FilePath -> [Int]
inp = map read . lines . unsafePerformIO . readFile

test, input :: [Int]
test = inp "day10-test.txt"
input = inp "day10-input.txt"
test2 = inp "day10-test2.txt"

main = do
    print part1test
    print part1actual
    print part2test
    print part2actual

part1test = "hi"
part1testval = 220

part1actual = "hi"
part2test = "hi"
part2actual = "hi"

-- input = output joltage of adapters
--
-- device needs max(all adapters) + 3
-- charger has rate 0
--

step (ones, threes, prev) nxt =
    if nxt - prev == 1
    then (ones + 1, threes, nxt)
    else
        if nxt - prev == 3
        then (ones, threes + 1, nxt)
        else error "Neither 1 nor 3!"

reduce :: [Int] -> (Int, Int, Int)
reduce = foldl' step (0,1,0) . sort


part1 = go . reduce
    where go (one, thr, _) = one * thr



-- list = a : (a : (a : z))

-- foldr = a `f` (a `f` (a `f` z))


step2 c [] = [(1,c)]
step2 c [(n, x)] = [(n, c), (n, x)]
step2 c r@[(n, x), (m, y)] = 
    if y - c > 3
    then (n, c) : r
    else (n + m, c) : r

step2 c r@((n, x) : (m, y) : (o, z) : _) =
    let can1 = True
        can2 = y - c <= 3
        can3 = z - c <= 3

    in if can3
        then (n + m + o, c) : r
        else
            if can2
            then  (n + m, c) : r
            else (n, c) : r


part2 :: [Int] -> [(Integer, Int)]
part2 = foldr step2 [] . (0 :) . sort
