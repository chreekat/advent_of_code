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

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day1-ex1.txt")
{-# NOINLINE ex2 #-}
ex2 = unsafePerformIO (readFile "day1-ex2.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day1.txt")

ans1 :: _
ans1 = sum
    . map (getCalibration . (\s -> (s,s)))
    $ lines dat

getCalibration :: (String, String) -> Int
getCalibration (s, s') = read [getL s, getR s']

getL = head . filter (`elem` "123456789")
getR = last . filter (`elem` "123456789")

ans2 :: _
ans2 = sum
    . map (getCalibration . (simplifyL &&& simplifyR))
    $ lines dat

simplifyL :: String -> String
simplifyL = go
  where
    go ('o':'n':'e':xs)         = '1':simplifyL xs
    go ('t':'w':'o':xs)         = '2':simplifyL xs
    go ('t':'h':'r':'e':'e':xs) = '3':simplifyL xs
    go ('f':'o':'u':'r':xs)     = '4':simplifyL xs
    go ('f':'i':'v':'e':xs)     = '5':simplifyL xs
    go ('s':'i':'x':xs)         = '6':simplifyL xs
    go ('s':'e':'v':'e':'n':xs) = '7':simplifyL xs
    go ('e':'i':'g':'h':'t':xs) = '8':simplifyL xs
    go ('n':'i':'n':'e':xs)     = '9':simplifyL xs
    go (x:xs)                   = x  :simplifyL xs
    go []                       = []

simplifyR :: String -> String
simplifyR = reverse . go . reverse
  where
    go ('e':'n':'o':xs)         = '1':go xs
    go ('o':'w':'t':xs)         = '2':go xs
    go ('e':'e':'r':'h':'t':xs) = '3':go xs
    go ('r':'u':'o':'f':xs)     = '4':go xs
    go ('e':'v':'i':'f':xs)     = '5':go xs
    go ('x':'i':'s':xs)         = '6':go xs
    go ('n':'e':'v':'e':'s':xs) = '7':go xs
    go ('t':'h':'g':'i':'e':xs) = '8':go xs
    go ('e':'n':'i':'n':xs)     = '9':go xs
    go (x:xs)                   = x  :go xs
    go []                       = []


-- ghcid needs this?
main = print ans2
