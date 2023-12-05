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
ex1 = unsafePerformIO (readFile "day4-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day4.txt")

pCard = do
    lexx (string "Card")
    c <- p_int
    lexx (p_char ':')
    winners <- many (lexx p_int)
    lexx (p_char '|')
    have <- many (lexx p_int)
    pure (c, winners, have)

score (_, winners, have) =
    let l = length (intersect winners have) - 1
    in if l < 0 then 0 else 2 ^ l

ans1 :: _
ans1 = sum $ map (score . parse' pCard) (lines dat)

initList = vecFromList $ map ((\(c,w,h) -> (1,w,h)) . parse' pCard) $ lines dat

score2 v i =
    let (n, w, h) = veci v i
        m = length (intersect w h)
        newCards =
            [ (i', (n+n', w', h'))
            | m > 0
            , i' <- [i+1..i+m]
            , i' < length v
            , let (n',w',h') = veci v i'
            ]
    in vecUpdate v newCards

g = foldl' score2 initList [0..length initList - 1]

ans2 :: _
ans2 = sum $ fmap (\(n,_,_) -> n) g

-- ghcid needs this?
main = print ans2
