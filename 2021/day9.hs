{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

import Tropes

ex1, dat :: String
ex1 = unsafePerformIO (readFile "day9-ex1.txt")
dat = unsafePerformIO (readFile "day9.txt")


type Coord = (Int,Int)

data Heights a = Heights Coord (Map Coord a)
    deriving (Eq, Show, Functor)

instance Comonad Heights where
    extract (Heights p m) = m ! p

    -- (w a -> b) -> w a -> w b
    extend f (Heights p m) = Heights p m'
        where
            m' = mapWithKey (\k _ -> f (Heights k m)) m

hmap (Heights p m) = m

mkHeights :: String -> Heights Int
mkHeights = Heights (0,0) . fmap (read . (:[])) . mapFromList . foldMap f . zip [0..] . map (zip [0..]) . lines
  where
    f (x, ys) = map (first (x,)) ys


neighbors (Heights (x,y) m) =
    [ (p,v)
    | x' <- [x-1,x,x+1]
    , y' <- [y-1,y,y+1]
    , x' /= x || y /= y'
    , let p = (x',y')
    , Just v <- [mapLookup p m]
    ]

lowest h@(Heights p m) =
    let ns = neighbors h
        v = m ! p
    in (v, all (> v) (map snd ns))

lowPoints :: _ -> [Int]
lowPoints = map fst . filter snd . toList . hmap . extend lowest

lowScore = sum . map (+1) . lowPoints

ans1 :: _
ans1 = lowScore (mkHeights dat)

ans2 :: _
ans2 = undefined

-- ghcid needs this?
main = undefined
