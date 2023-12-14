{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant map" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use tuple-section" #-}

{-# LANGUAGE FlexibleContexts #-}



{-# LANGUAGE PartialTypeSignatures #-}


{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Tropes
import TwoD
import qualified Data.Array as Array
import Data.List

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day11-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day11.txt")

emptyCol g y = notElem '#' [g Array.! (x, y) | x <- [x1 .. x2]]
  where
    ((x1, _), (x2, _)) = Array.bounds g

emptyRow g x = notElem '#' [g Array.! (x, y) | y <- [y1 .. y2]]
  where
    ((_, y1), (_, y2)) = Array.bounds g

duplicateEmptyCol y g | emptyCol g y = insertCol' y (repeat '|') g
                      | otherwise = g

duplicateEmptyRow x g | emptyRow g x = insertRow' x (repeat '-') g
                      | otherwise = g

expandGrid g = foldr duplicateEmptyRow `flip` [x1 .. x2] $ foldr duplicateEmptyCol `flip` [y1 .. y2] $ g
    where ((x1,y1), (x2,y2)) = Array.bounds g


stars :: Array.Ix i => Array.Array i Char -> [i]
stars = map fst . filter ((== '#') . snd) . Array.assocs

starDistances (star:rest) = map (distance star) rest
starDistances [] = []

allStarDistances = map starDistances . tails

ans1 :: _
ans1 d = sum $ concat $ allStarDistances $ stars $ expandGrid $ twoD d

emptyRows g = filter (emptyRow g) [x1 .. x2]
  where
    ((x1, _), (x2, _)) = Array.bounds g

emptyCols g = filter (emptyCol g) [y1 .. y2]
  where
    ((_, y1), (_, y2)) = Array.bounds g

muchlyExpandEmptyRow n x stars = before <> after
    where
        (before, after') = partition ((< x) . fst) stars
        after = map (first (+n)) after'

muchlyExpandEmptyCol n y stars = before <> after
    where
        (before, after') = partition ((< y) . snd) stars
        after = map (second (+n)) after'

muchlyExpandGrid n rs cs stars = foldr (muchlyExpandEmptyCol n) (foldr (muchlyExpandEmptyRow n) stars rs) cs


ans2 :: _
ans2 d = let
    g = twoD d
    rs = emptyRows g
    cs = emptyCols g
    stars' = stars g
    in sum $ concat $ allStarDistances $ muchlyExpandGrid 999999 rs cs stars'

main = print $ ans2 dat
