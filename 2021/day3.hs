{-# LANGUAGE PartialTypeSignatures #-}

import Data.Bits
import Tropes

ex1 =
    unlines
        [ "00100"
        , "11110"
        , "10110"
        , "10111"
        , "10101"
        , "01111"
        , "00111"
        , "11100"
        , "10000"
        , "11001"
        , "00010"
        , "01010"
        ]

dat :: String
dat = unsafePerformIO (readFile "day3.txt")

-- gamma = most common
-- epsilon = least rommon
-- pow = gamma * epsilon

step (sum', count') = go
  where
    go '0' = (sum', count' + 1)
    go '1' = (sum' + 1, count' + 1)

gammaS = map (simp . foldl' step (0, 0)) . transpose . lines
  where
    simp (s, c) = case c `div` s of
        1 -> '1'
        2 -> '0'

swapBit '1' = '0'
swapBit '0' = '1'

toDigit :: String -> Int
toDigit =
    sum
        . map toBit
        . zip [0 ..]
        . reverse

fromDigit d =
    let ch d' = case testBit d' 0 of
            True -> '1'
            False -> '0'
        chs d' = if d' == 0 then "" else ch d' : (chs (shiftR d' 1))
     in reverse (chs d)

toBit (n, v) = case v of
    '0' -> 0
    '1' -> bit n

epS = map swapBit . gammaS

pow f = product (map toDigit [gammaS f, epS f])

ans1 :: _
ans1 = pow dat

gammaS' = map (simp . foldl' step (0, 0)) . transpose
  where
    simp (s, c) = case compare (2 * s) c of
        LT -> '0'
        GT -> '1'
        EQ -> '1'

epS' = map swapBit . gammaS'

o2GenRating = rating gammaS'
rating func rep =
    let inputs = lines rep
        sz = length (head inputs)

        filter' i '0' = filter (not . flip testBit i . toDigit)
        filter' i '1' = filter (flip testBit i . toDigit)

        step i r =
            let g' = reverse (func r)
             in traceShow r $
                    case filter' i (g' !! i) r of
                        [] -> error "No results remain"
                        [x] -> x
                        xs -> step (i -1) xs
     in toDigit $ step (sz -1) inputs

co2ScrubRating = rating epS'

lifeSup rep = o2GenRating rep * co2ScrubRating rep

ans2 :: _
ans2 = lifeSup dat

-- ghcid needs this?
main = undefined
