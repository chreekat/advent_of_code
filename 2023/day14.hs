{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant map" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Tropes

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day14-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day14.txt")

step (loc, nextVal, sum) = \case
    'O' -> (pred loc, pred nextVal, sum + nextVal)
    '#' -> (pred loc, pred loc, sum)
    '.' -> (pred loc, nextVal, sum)

row xs = _3of3 $ foldl' step (length xs, length xs, 0) xs

ans1 :: _
ans1 = sum $ map row $ transpose $ lines dat

ans2 :: _
ans2 = undefined

main = print ans1
