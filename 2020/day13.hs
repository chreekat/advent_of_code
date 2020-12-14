{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Arrow ((&&&))
import Data.Bifunctor
import Data.Foldable
import Data.Function
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import System.IO.Unsafe
import qualified Data.Vector as V

import SimpleParse

import Debug.Pretty.Simple

test = unsafePerformIO (readFile "day13-test.txt")
input = unsafePerformIO (readFile "day13-input.txt")

main = do
    print part1test
    print part1actual
    print part2test
    print part2actual

part1test = "hi"
part1actual = "hi"
part2test = "hi"
part2actual = "hi"


nextDeparture now x = x - (now `mod` x)

pInput :: String -> (Int, [Int])
pInput x =
    let [now, crud] = lines x
    in (read now, map read (filter (/= "x") ((splitOn "," crud))))



part1 inp = 
    let (now, buses) = pInput inp
        (quickBus, nextDep) = head $ sortBy (compare `on` snd)  $ map (id &&& nextDeparture now) buses
    in pTraceShow (quickBus, nextDep) $ quickBus * nextDep

sub (x, y) = y - x

pInput2 :: String -> [(Int, Int)]
pInput2
    = reverse
    . sortBy (compare `on` sub)
    . map (second read)
    . filter ((/= "x") . snd)
    . zip [0..]
    . splitOn ","
    . last
    . lines


-- blah (4, 59) (6, 31) 1 = if (1 * 59 + (6 - 4)) `mod` 31 == 0 then (1 * 59 + (6 -4), 59 * 31)
--                                    else blah (4, 59) (6,31) 2


blah2 (r1, a1) (r2, a2) i =
    let r3 = i * a1 - r1
    in
        if (r3 + r2) `mod` a2 == 0
        then Just (-r3, a1 * a2)
        else Nothing

blah x y = 
    let f = blah2 x y
    in fromJust $ getFirst ((foldMap (First . f)) [0..])

part2 = negate . fst . foldl1' blah . pInput2

earliest :: [(Int, Int)] -> Int
earliest = foldl' (\x y -> lcm x (sub y)) 1
--}
