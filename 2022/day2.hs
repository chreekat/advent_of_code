{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Tropes

ex1, dat :: String
ex1 = unsafePerformIO (readFile "day2-ex1.txt")
dat = unsafePerformIO (readFile "day2.txt")

sc "A" "X" = 1 + 3
sc "A" "Y" = 2 + 6
sc "A" "Z" = 3 + 0
sc "B" "X" = 1 + 0
sc "B" "Y" = 2 + 3
sc "B" "Z" = 3 + 6
sc "C" "X" = 1 + 6
sc "C" "Y" = 2 + 0
sc "C" "Z" = 3 + 3

pick "A" "X" = 0 + 3
pick "A" "Y" = 3 + 1
pick "A" "Z" = 6 + 2
pick "B" "X" = 0 + 1
pick "B" "Y" = 3 + 2
pick "B" "Z" = 6 + 3
pick "C" "X" = 0 + 2
pick "C" "Y" = 3 + 3
pick "C" "Z" = 6 + 1

ans1 :: _
ans1
    = sum
    . map (\[a, b] -> sc a b)
    . map words
    . lines


ans2 :: _
ans2
    = sum
    . map (\[a, b] -> pick a b)
    . map words
    . lines

-- ghcid needs this?
main = undefined
