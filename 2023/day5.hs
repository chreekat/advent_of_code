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
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}

import Tropes
import SimpleParse
import Control.Parallel.Strategies

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day5-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day5.txt")


p_seeds = do
    lexx (p_string "seeds:")
    many (lexx p_int)

pMap = map (parse' (many (lexx p_int))) . tail . lines

pDat d = let (s : maps) = splitOn "\n\n" d
    in (parse' p_seeds s, map pMap maps)

apMap i [d,s,l] | i >= s && i < s+l = Just $ d + (i-s)
                | otherwise = Nothing

apMultiMap i maps = head $ mapMaybe (apMap i) maps <> [i]

graph dat s = foldl' apMultiMap s dat

ans1 :: _
ans1 = let (seeds, maps) = pDat dat in minimum $ map (graph maps) seeds

p2_seeds = chunksOf 2 <$> p_seeds

p2Dat d = let (s : maps) = splitOn "\n\n" d
    in (parse' p2_seeds s, map pMap maps)

allSeeds = concatMap (\[init, length] -> [init..init+length-1])

{-# NOINLINE p2dat #-}
p2dat = p2Dat dat

ans2 :: _
-- ans2 = let (seeds, maps) = p2dat in minimum $ map (graph maps) (allSeeds seeds)
ans2 = let (seeds, maps) = p2dat in
    minimum $ withStrategy (parBuffer 1000 rseq) $ map (graph maps) (allSeeds seeds)

main = print ans2
