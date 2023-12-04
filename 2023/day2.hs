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
{-# OPTIONS_GHC -Wno-deprecations #-}

import Tropes
import SimpleParse

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day2-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day2.txt")

ans1 :: _
ans1 =
    let res = map (parse' pLine) $ lines dat
    in print $ sum $ mapMaybe possGame res

possible (n, "red")   = n <= 12
possible (n, "green") = n <= 13
possible (n, "blue")  = n <= 14

possGame (g, samps) | all (all possible) samps = Just g
                    | otherwise = Nothing


pLine = do
    p_string "Game "
    g <- p_int
    p_string ": "
    samps <- samp `sepBy` p_string "; "
    pure (g, samps)

samp = grp `sepBy` p_string ", "

grp = do
    n <- p_int
    p_char ' '
    c <- p_string "blue" <|> p_string "red" <|> p_string "green"
    pure (n, c)
    
-- res is a list of games
-- a game is a list of samples
-- a sample is a list of colors
--
-- map f over the colors to get the max for a sample
-- map max3 over the samples to get the max for a game
-- map pwr over the games and then sum
-- or
-- sum over
-- map pwr over
-- map (
ans2 :: _
ans2 =
    let res = map (parse' pLine) $ lines dat
    in print $ sum $ j res

maximum3 :: [(Int, Int, Int)] -> (Int, Int, Int)
maximum3 = foldr max3 (0,0,0)
(a,b,c) `max3` (x,y,z) = (max a x, max b y, max c z)

-- minsamps = foldr min3 (0,0,0)

pwr (r,g,b) = product [r,g,b]

j = map (pwr . i)

i (_, ss) = h ss

h :: [[(Int, String)]] -> (Int, Int, Int)
h = maximum3 . map g

g :: [(Int, String)] -> (Int, Int, Int)
g = foldr f (0,0,0)

f (n, "red")   (r,g,b) = (max r   n,  g, b)
f (n, "green") (r,g,b) = (r,  max g   n, b)
f (n, "blue")  (r,g,b) = (r,  g,  max b  n)



-- ghcid needs this?
main = ans2
