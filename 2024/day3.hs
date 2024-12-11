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
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Data.Array qualified as Array
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence qualified as Seq
import Data.Vector qualified as Vector
import Text.Read qualified as Read
import Tropes

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day3-ex1.txt")
{-# NOINLINE ex2 #-}
ex2 = unsafePerformIO (readFile "day3-ex2.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day3.txt")


f [] = 0
f ('m':'u':'l':'(':xs)
    | (Read.readMaybe -> Just n, ',':xs') <- span (/= ',') xs
    , (Read.readMaybe -> Just m, ')':xs'') <- span (/= ')') xs'
    = n * m + f xs''
f (_:xs) = f xs

g ([],_) = 0
g ('d':'o':'n':'\'' : 't': '(' : ')': xs, _) = g' (xs,0)
g ('d':'o':'(':')':xs, _) = g' (xs,1)
g ('m':'u':'l':'(':xs, 1)
    | (Read.readMaybe -> Just n, ',':xs') <- span (/= ',') xs
    , (Read.readMaybe -> Just m, ')':xs'') <- span (/= ')') xs'
    = n * m + g' (xs'', 1)
g (_:xs, a) = g' (xs, a)

g' = g

ans1 :: _
ans1 = f dat

ans2 :: _
ans2 = g (dat,1)

main = print ans2
