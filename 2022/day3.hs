{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Tropes

ex1, dat :: String
ex1 = unsafePerformIO (readFile "day3-ex1.txt")
dat = unsafePerformIO (readFile "day3.txt")

shared :: String -> Char
shared
    = head
    . uncurry intersect
    . (\s -> 
        let n = length s
        in splitAt (n `div` 2) s)

score :: Char -> Int
score c
    = (1 +)
    . fromJust
    . elemIndex c
    $ ['a' .. 'z'] ++ ['A' .. 'Z']

ans1 :: _
ans1
    = sum
    . map (score . shared)
    . lines

ans2 :: _
ans2 = undefined

-- ghcid needs this?
main = undefined
