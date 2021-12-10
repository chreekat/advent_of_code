{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}

import Tropes

ex1, dat :: String
ex1 = unsafePerformIO (readFile "day9-ex1.txt")
dat = unsafePerformIO (readFile "day9.txt")

type Coord = (Int, Int)

data Heights a = Heights Coord (Map Coord a)
    deriving (Eq, Show, Functor)

instance Comonad Heights where
    extract (Heights p m) = m ! p

    -- (w a -> b) -> w a -> w b
    extend f (Heights p m) = Heights p m'
      where
        m' = mapWithKey (\k _ -> f (Heights k m)) m

hmap (Heights p m) = m

hpoint (Heights p m) = p

hval (Heights p m) = m ! p

mkHeights :: String -> Heights Int
mkHeights = Heights (0, 0) . fmap (read . (: [])) . mapFromList . foldMap f . zip [0 ..] . map (zip [0 ..]) . lines
  where
    f (x, ys) = map (first (x,)) ys

neighbors (Heights (x, y) m) =
    [ (p, v)
    | x' <- [x -1, x, x + 1]
    , y' <- [y -1, y, y + 1]
    , x' /= x || y /= y'
    , x' == x || y' == y
    , let p = (x', y')
    , Just v <- [mapLookup p m]
    ]

lowest h@(Heights p m) =
    let ns = neighbors h
        v = m ! p
     in (v, all (> v) (map snd ns))

lowPoints :: _ -> [Int]
lowPoints = map fst . filter snd . toList . hmap . extend lowest

lowScore = sum . map (+ 1) . lowPoints

ans1 :: _
ans1 = lowScore (mkHeights dat)

lowPoints' :: _ -> [Heights Int]
lowPoints' h@(Heights p m) =
    let ks = keys . mapFilter snd . hmap . extend lowest $ h
     in map (`Heights` m) ks

largerNeighbors h@(Heights p m) =
    let ns = neighbors h
        v = m ! p
     in map (flip Heights m . fst) $ filter ((\v' -> v' < 9 && v' >= v) . snd) ns

basinNeighbors bs h@(Heights p m) =
    let ls = largerNeighbors h
     in ls \\ bs

basin :: Heights Int -> [Heights Int]
basin h = go [h] h
  where
    go bs h = case basinNeighbors bs h of
        [] -> bs
        -- FIXME: use a fold to avoid expensive nubbing
        xs -> nub $ concatMap (go (bs <> xs)) xs

basinScore =
    product . take 3 . reverse . sort . map length . map basin . lowPoints' . mkHeights

ans2 :: _
ans2 = basinScore dat

-- ghcid needs this?
main = undefined
