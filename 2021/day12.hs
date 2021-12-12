{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}

import Tropes

ex1, dat :: String
ex1 = unsafePerformIO (readFile "day12-ex1.txt")
dat = unsafePerformIO (readFile "day12.txt")

data V a = V
    { edges :: Set a
    } deriving (Eq, Show)

instance (Ord a, Semigroup a) => Semigroup (V a) where
    V s1 <> V s2 = V (s1 <> s2)

data G a = G (Map String (V a))

mkSegs = map (splitOn "-") . lines

mkG = mapUnionsWith (<>) . map f . mkSegs where
    f [a, b] =
        mapFromList
            [ (a, V (setFromList [b]))
            , (b, V (setFromList [a]))
            ]

ans1 :: _
ans1 = undefined

ans2 :: _
ans2 = undefined

-- ghcid needs this?
main = undefined
