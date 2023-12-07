{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Redundant map" #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use infix" #-}

import Tropes
import SimpleParse

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day7-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day7.txt")

cardStrength = fromJust . flip lookup l where l = zip "23456789TJQKA" [1..]

handStrength f = go
    where
        grps = group . sort $ f
        c = cardStrength (head f)
        go | length grps == 5 = 0
           -- ^ high card
           | length grps == 4 = 1
           -- ^ pair
           | length grps == 3 && maximum (map length grps) == 2 = 2
           -- ^ two pair
           | length grps == 3 = 3
           -- ^ three of a kind
           | length grps == 2 && maximum (map length grps) == 3 = 4
           -- ^ full house
           | length grps == 2 = 5
           -- ^ four of a kind
           | otherwise = 6
           -- ^ five of a kind

compareHand h c a b =
    (compare `on` h) a b
    <> mconcat (zipWith (compare `on` c) a b)

pHands = map (parse' p) . lines where
    p = (,) <$> many (noneOf " ") <* p_string " " <*> p_int

rankHands h c = sortBy (compareHand h c `on` fst)
scoreHands h c = sum . zipWith (*) [1..] . map snd . rankHands h c

ans1 :: _
ans1 = scoreHands handStrength cardStrength $ pHands dat

cardStrength2 = fromJust . flip lookup l where l = zip "J23456789TQKA" [1..]

high = 0
pair = 1
twopair = 2
three = 3
full = 4
four = 5
five = 6

handStrength2 f = go
    where
        (length -> numJokers, group . sort -> grps) = partition (== 'J') f
        biggestGrp = maximum (0: map length grps)
        c = cardStrength2 (head f)
        go
            | biggestGrp == 1
            = case numJokers of
                0 -> high
                1 -> pair
                2 -> three
                3 -> four
                4 -> five
            | biggestGrp == 2
            , length (filter (> 1) (map length grps)) == 1
            = case numJokers of
                0 -> pair
                1 -> three
                2 -> four
                3 -> five
            -- ^ pair. No jokers = pair, jokers = twopair + numJokers
            | biggestGrp == 2
            = case numJokers of
                0 -> twopair
                1 -> full
            -- ^ twopair
            | biggestGrp == 3
            , length (filter (> 1) (map length grps)) == 1
            = case numJokers of
                0 -> three
                1 -> four
                2 -> five
            -- ^ three of a kind
            | biggestGrp == 3
            = case numJokers of
                0 -> full
                1 -> four
            -- ^ full house
            | biggestGrp == 4
            = case numJokers of
                0 -> four
                1 -> five
            -- ^ four of a kind
            | otherwise = five

ans2 :: _
ans2 = scoreHands handStrength2 cardStrength2 $ pHands dat

main = print ans2
