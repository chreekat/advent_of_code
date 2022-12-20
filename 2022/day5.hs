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
ex1 = unsafePerformIO (readFile "day5-ex1.txt")
dat = unsafePerformIO (readFile "day5.txt")

-- This has a bug: it won't pick up empty stacks.
stacks = filter (not . null) . map (filter (`elem` ['A'..'Z'])) . transpose . lines . head . pars

prse inp =
    let [p1, p2] = pars inp
        ss = vecFromList (stacks p1)
        moves = map (parse' pMove) (lines p2)
    in (ss, moves)


mkMove' f ss (n, s', d') =
    let s = s' - 1
        d = d' - 1
        (a', sv) = splitAt n (veci ss s)
        a = f a'
        dv = a <> veci ss d
        ss' = vecUpdate ss [(s, sv), (d, dv)]
    in traceShow (a, sv, dv, ss') $ ss'

mkMove = mkMove' reverse
mkMove2 = mkMove' id

pMove = do
    _ <- sym "move"
    n <- integer
    _ <- sym "from"
    s <- integer
    _ <- sym "to"
    d <- integer
    pure (n, s, d)

ans1 :: _
ans1 x = 
    let (a, b) = prse x
        res = foldl' mkMove a b
    in foldMap (take 1) res

ans2 :: _
ans2 x = 
    let (a, b) = prse x
        res = foldl' mkMove2 a b
    in foldMap (take 1) res


-- ghcid needs this?
main = undefined
