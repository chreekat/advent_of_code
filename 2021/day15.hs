{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Tropes

ex1, dat :: String
ex1 = unsafePerformIO (readFile "day15-ex1.txt")
dat = unsafePerformIO (readFile "day15.txt")


type Coord = (Int, Int)

data G = G
    { gg :: Map Coord V
    , gsz :: Int
    } deriving (Eq, Show)

newtype D a = D { unD :: Maybe a } deriving (Eq, Show, Functor, Applicative)

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
    , visited :: Bool
    } deriving (Eq, Show)

mkV w = V w maxBound False

visit v = v { visited = True }
score s v = v { vscore = D (Just s) }

mkG :: Coord -> String -> G
mkG start s =
    let ys = lines s
        as = concat $ zipWith (\y -> zipWith (\x e -> ((x,y), read (e:[]))) [0..]) [0..] ys
        vs = map (second mkV) as
    in G (mapAdjust (score 0) start (mapFromList vs)) (length ys)

data St = St
    { stg :: G
    , unvisited :: Set Coord
    , target :: Coord
    } deriving (Eq, Show)

mkSt :: String -> St
mkSt s =
    let g = mkG (0,0) s
    in St g (mapKeysSet (gg g)) (gsz g - 1, gsz g - 1)

leastV (St (G m _) unvisited _) =
    let c = minimumBy (compare `on` (vscore . (m !))) unvisited
    in (c, m ! c)

neighbors (x,y) m =
    catMaybes [((x2,y2),) <$> mapLookup (x2,y2) m
              | x2 <- [x-1,x,x+1]
              , y2 <- [y-1,y,y+1]
              , x2 /= x || y2 /= y
              , x2 == x || y2 == y
              ]

adjustG :: (V -> V) -> Coord  -> G -> G
adjustG f c g = g { gg = mapAdjust f c (gg g) }

adjustSt f c st = st { stg = adjustG f c (stg st) }

insertG c v g = g { gg = mapInsert c v (gg g) }

insertSt c v st = st { stg = insertG c v (stg st) }

procN this (n, neigh) =
    let newScore = (+) <$> vscore this <*> fin (vweight neigh)
        oldScore = vscore neigh
    in modify (insertSt n neigh { vscore = min newScore oldScore })

step :: State St _
step = do
    ((c, this),target) <- gets (leastV &&& target)
    if c == target
        then pure this
        else do
            ns <- gets (neighbors c . gg . stg)
            traverse_ (procN this) ns
            modify (\st -> st { unvisited = setDelete c (unvisited st) })
            modify (adjustSt visit c)
            step

ans1 :: Int
ans1 = fromJust . unD . vscore $ evalState step (mkSt dat)

ans2 :: Int
ans2 = undefined

-- ghcid needs this?
main = print ans1
