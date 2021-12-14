{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Tropes

ex1, dat :: String
ex1 = unsafePerformIO (readFile "day14-ex1.txt")
dat = unsafePerformIO (readFile "day14.txt")


data I = I { is :: String, im :: [(String, Char)] }
    deriving (Eq, Show)

instance Semigroup I where
    I s m <> I s2 m2 = I (s <> s2) (m <> m2)

instance Monoid I where
    mempty = I mempty mempty

pTmpl s = I s mempty

pRule s = 
    let [k, [v]] = splitOn " -> " s
    in I "" [(k, v)]

pI s =
    let [[tmpl], rules] = splitOn [""] $ lines s
    in mconcat (pTmpl tmpl : map pRule rules)

ans1 :: _
ans1 = undefined

ans2 :: _
ans2 = undefined

-- ghcid needs this?
main = undefined
