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


data I = I { is :: String, im :: Map String Char }
    deriving (Eq, Show)

instance Semigroup I where
    I s m <> I s2 m2 = I (s <> s2) (m <> m2)

instance Monoid I where
    mempty = I mempty mempty

pTmpl s = I s mempty

pRule s = 
    let [k, [v]] = splitOn " -> " s
    in I "" (mapSingleton k v)

pI s =
    let [[tmpl], rules] = splitOn [""] $ lines s
    in mconcat (pTmpl tmpl : map pRule rules)

step :: I -> _
step I {is, im} = I newIs im where
    newIs = head is : mconcat (zipWith (expand im) is (tail is))

expand :: Map String Char -> Char -> Char -> String
expand m c1 c2 = maybe [c2] (: [c2]) (mapLookup [c1,c2] m)

score  I { is } = 
    let m = mapUnionsWith (<>) (map (\c -> mapSingleton c (Sum 1)) is)
        (j, k) = (minimum &&& maximum) m
    in getSum $ k - j

scoreSteps n s = score $ last $ take (n+1) $ iterate step (pI s)

ans1 :: _
ans1 = scoreSteps 10 dat

ans2 :: _
ans2 = undefined

-- ghcid needs this?
main = undefined
