{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Tropes

import qualified Data.HashPSQ as HashPQ

ex1, dat :: String
ex1 = unsafePerformIO (readFile "day15-ex1.txt")
dat = unsafePerformIO (readFile "day15.txt")

type Coord = (Int, Int)

data G = G
    { gg :: Map Coord V
    , gsz :: Int
    }
    deriving (Eq, Show)

newtype D a = D {unD :: Maybe a} deriving (Eq, Show, Functor, Applicative)

instance Ord a => Ord (D a) where
    D Nothing <= D (Just _) = False
    D (Just _) <= D Nothing = True
    D x <= D y = x <= y

instance Bounded a => Bounded (D a) where
    maxBound = D Nothing
    minBound = D (Just minBound)

fin = D . Just

data V = V
    { vweight :: Int
    , vscore :: D Int
    }
    deriving (Eq, Show)

instance Ord V where
    compare = compare `on` vscore

mkV w = V w maxBound

-- visit v = v { visited = True }
score s v = v{vscore = D (Just s)}

mkG :: Coord -> String -> G
mkG start s =
    let ys = lines s
        as = concat $ zipWith (\y -> zipWith (\x e -> ((x, y), read (e : []))) [0 ..]) [0 ..] ys
        vs = map (second mkV) as
     in G (mapAdjust (score 0) start (mapFromList vs)) (length ys)

data St = St
    { stg :: G
    , unvisited :: HashPQ.HashPSQ Coord V V
    , target :: Coord
    }
    deriving (Eq, Show)

mkSt :: String -> St
mkSt s =
    let g = mkG (0, 0) s
        addP k v = (k, v, v)
     in St g (HashPQ.fromList (map (uncurry addP) (mapToList (gg g)))) (gsz g - 1, gsz g - 1)

-- O(|unvisited| * log |m|)
-- Ok, this one is *ALL* the time.
-- Update: Fixed with PSQs!
leastV (St (G m _) unvisited _) =
    let Just (k, _, v) = HashPQ.findMin unvisited
     in (k, v)

-- O(4 * log |m|)
neighbors (x, y) m =
    catMaybes
        [ ((x2, y2),) <$> mapLookup (x2, y2) m
        | x2 <- [x -1, x, x + 1]
        , y2 <- [y -1, y, y + 1]
        , x2 /= x || y2 /= y
        , x2 == x || y2 == y
        ]

-- O(log |g|)
adjustG :: (V -> V) -> Coord -> G -> G
adjustG f c g = g{gg = mapAdjust f c (gg g)}

adjustSt f c st = st{stg = adjustG f c (stg st)}

-- O(log |g|)
insertG c v g = g{gg = mapInsert c v (gg g)}

insertSt c v st = st{stg = insertG c v (stg st)}

-- O(log |g|)
procN this (n, neigh) =
    let newScore = (+) <$> vscore this <*> fin (vweight neigh)
        oldScore = vscore neigh
        newNeigh = neigh{vscore = min newScore oldScore}
        pingQ Nothing = ((), Nothing)
        pingQ (Just _) = ((), Just (newNeigh, newNeigh))
     in do
            modify (insertSt n neigh{vscore = min newScore oldScore})
            modify (\st -> st{unvisited = snd (HashPQ.alter pingQ n (unvisited st))})

-- O(|unv| * log |g| + 4*log |g| + 4*log |g| + log |unv|)
--
-- Actually I guess that's as good as it gets with Data.Map.Map.
-- Update: It doesn't get much better with HashMap.
step :: State St _
step = do
    ((c, this), target) <- gets (leastV &&& target)
    if c == target
        then pure this
        else do
            ns <- gets (neighbors c . gg . stg)
            traverse_ (procN this) ns
            modify (\st -> st{unvisited = HashPQ.delete c (unvisited st)})
            -- modify (adjustSt visit c)
            step

ans1 :: Int
ans1 = fromJust . unD . vscore $ evalState step (mkSt dat)

ans2 :: Int
ans2 = undefined

-- ghcid needs this?
main = print ans1
