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
ex1 = unsafePerformIO (readFile "day9-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day9.txt")


pVals = map (parse' (many signedInt)) . lines

test ends xs | all (== 0) xs = sum ends
             | otherwise = 
                    let r = zipWith (-) (tail xs) xs
                    in test (last xs:ends) r

ans1 :: _
ans1 = sum $ map (test []) $ pVals dat

test2 ends xs | all (== 0) xs = foldl' (flip (-)) 0 ends
              | otherwise = 
                    let r = zipWith (-) (tail xs) xs
                    in test2 (head xs:ends) r

ans2 :: _
ans2 = sum $ map (test2 []) $ pVals dat

main = print ans2
