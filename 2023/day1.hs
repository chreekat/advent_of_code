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
    . map getCalibration
    $ lines dat

getCalibration :: String -> Int
getCalibration = (\(First (Just f), Last (Just l)) -> read [f,l])
    . foldl' bump (First Nothing, Last Nothing)
  where
    bump (f, l) x | x `elem` "123456789" = (f <> pure x, l <> pure x)
                  | otherwise = (f, l)

ans2 :: _
ans2 = sum
    . map (pTraceShowId . getCalibration . pTraceShowId . simplify)
    $ lines dat

simplify :: String -> String
simplify = go . pTraceShowId
  where
    go ('o':'n':'e':xs)         = '1':simplify xs          
    go ('t':'w':'o':xs)         = '2':simplify xs          
    go ('t':'h':'r':'e':'e':xs) = '3':simplify xs          
    go ('f':'o':'u':'r':xs)     = '4':simplify xs          
    go ('f':'i':'v':'e':xs)     = '5':simplify xs          
    go ('s':'i':'x':xs)         = '6':simplify xs          
    go ('s':'e':'v':'e':'n':xs) = '7':simplify xs          
    go ('e':'i':'g':'h':'t':xs) = '8':simplify xs          
    go ('n':'i':'n':'e':xs)     = '9':simplify xs          
    go (x:xs)                   = x  :simplify xs
    go []                       = []                       

-- ghcid needs this?
main = print ans2
