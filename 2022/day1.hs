{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Tropes

ex1, dat :: String
ex1 = unsafePerformIO (readFile "day1-ex1.txt")
dat = unsafePerformIO (readFile "day1.txt")

ans1 :: _ -> Int
ans1
    = maximum
    . map (sum . map read)
    . map words
    . pars

ans2 :: _
ans2
    = sum
    . take 3
    . reverse
    . sort
    . map (sum . map read)
    . map words
    . pars

-- ghcid needs this?
main = print (ans1 dat) >> print (ans2 dat)
