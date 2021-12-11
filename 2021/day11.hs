{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}

import Tropes

ex1, ex2, dat :: String
ex1 = unsafePerformIO (readFile "day11-ex1.txt")
ex2 = unsafePerformIO (readFile "day11-ex2.txt")
dat = unsafePerformIO (readFile "day11.txt")

type Pt = (Int, Int)

type Fl = (Bool, Int)

newtype Nap a = Nap {getNap :: (Int, (Map Pt a))}
    deriving (Eq, Show, Functor)

data Nap' a = Nap'
    { flashes :: Sum Int
    , sz :: Int
    , getNap' :: Map Pt a
    }
    deriving (Eq, Show, Functor)

adjMap f (Nap (i, m)) = Nap (i, f m)

mkNap :: String -> Nap Fl
mkNap =
    Nap
        . ( length
                &&& mapFromList
                    . map (second (False,) . unassoc)
                    . concatMap (\(x, ys) -> map (x,) ys)
                    . zip [0 ..]
                    . map (zip [0 ..] . map (read . (: [])))
          )
        . lines

mkNap' s =
    let Nap (i, n) = mkNap s in Nap' (Sum 0) i n

proc pt = do
    -- pTraceShow pt $ do
    (b, v) <- gets ((! pt) . snd . getNap)
    if b
        then pure ()
        else do
            let v' = v + 1
            if v' > 9
                then do
                    flash pt
                    traverse_ proc =<< gets (neighbors pt)
                else modify (adjMap (mapInsert pt (False, v')))

printNap (Nap (i, m)) =
    let row x = concatMap (show . snd . (m !) . (x,)) [0 .. i -1]
     in putStr $ unlines (map row [0 .. i -1])

printNap' (i, n) = do
    print i
    printNap n

step = do
    i <- gets (fst . getNap)
    traverse_ proc (all i)
    flashes <- gets (mapSize . mapFilter fst . snd . getNap)
    modify (adjMap resetNap)
    pure flashes
  where
    all i = [(x, y) | x <- [0 .. i -1], y <- [0 .. i -1]]

resetNap :: _
resetNap = fmap (first (&& False))

neighbors :: Pt -> Nap Fl -> [Pt]
neighbors (x, y) (Nap (i, _)) =
    [ (x', y')
    | x' <- [x -1, x, x + 1]
    , y' <- [y -1, y, y + 1]
    , x' >= 0 && x' < i
    , y' >= 0 && y' < i
    , x' /= x || y /= y'
    ]

-- unilaterally flash pt
flash :: Pt -> State (Nap Fl) ()
flash pt = modify (adjMap (mapInsert pt (True, 0)))

-- proc (pt, nap) flashes at point pt, increments neighbors, and returns new nap
-- plus list of neighbors who will also flash.
--
-- To do that, we need to
-- - do nothing if pt is flashed already
-- - do nothing if pt shouldn't flash
-- - otherwise
-- - flash pt
-- - increment neighbors if they haven't been flashed
-- - collect list of neighbors who still need to flash

run n = runState (replicateM n step) . mkNap

ans1 :: _
ans1 = sum . fst $ run 100 dat

ans2 :: _
ans2 = undefined

-- ghcid needs this?
main = undefined
