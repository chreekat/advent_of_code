{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Tropes

--
-- ANOTHER GRID, HERE WE GO AGAIN
--

type Coord = (Int, Int)
data G = G
    { gg :: Map Coord GSpot
    } deriving (Eq, Show)

data GSpot = GSpot
    { gsrisk :: Int
    , gsscore :: Sum Int
    } deriving (Eq, Show)

readc :: Read a => Char -> a
readc = read . (:[])

pG :: String -> G
pG s = 
    let xs = zip [0..] $ lines s
        yss = map (second $ zip [0..] . map readc) xs
        f (x, ys) = map (unassoc . (x,)) ys
        g = mapFromList . map (second (\r -> GSpot r 0))
    in G $ g $ foldMap f yss

ex1, dat :: String
ex1 = unsafePerformIO (readFile "day15-ex1.txt")
dat = unsafePerformIO (readFile "day15.txt")

ans1 :: _
ans1 = undefined

ans2 :: _
ans2 = undefined

-- ghcid needs this?
main = undefined
