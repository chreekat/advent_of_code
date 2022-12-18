{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Tropes
import SimpleParse

ex1, dat :: String
ex1 = unsafePerformIO (readFile "day4-ex1.txt")
dat = unsafePerformIO (readFile "day4.txt")


pAssignment = do
    a <- integer
    _ <- p_char '-'
    b <- integer
    pure (a,b)

pPair = do
    f <- pAssignment
    _ <- p_char ','
    g <- pAssignment
    pure (f,g)

contains (a, b) (m, n)
    = a <= m && b >= n
    || m <= a && n >= b


overlap (a, b) (m, n)
    = a <= m && b >= m
    || a <= n && b >= n
    || m <= a && n >= a
    || m <= b && n >= b

ans1 :: _
ans1
    = length
    . filter (uncurry contains)
    . map (parse' pPair)
    . lines

ans2 :: _
ans2
    = length
    . filter (uncurry overlap)
    . map (parse' pPair)
    . lines

-- ghcid needs this?
main = undefined
