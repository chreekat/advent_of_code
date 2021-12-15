{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Tropes

ex1, dat :: String
ex1 = unsafePerformIO (readFile "day15-ex1.txt")
dat = unsafePerformIO (readFile "day15.txt")

--
-- ANOTHER GRID, HERE WE GO AGAIN
--

type Coord = (Int, Int)
data G = G
    { gg :: Map Coord V
    , gsz :: Int
    }
    deriving (Eq, Show)

type PointedG = (Coord, G)

data V = V
    { vrisk :: Int
    , vscore :: Int
    , vnext :: Maybe Coord
    }
    deriving (Eq, Show)

data St = St
    { stg :: G
    }
    deriving (Eq, Show)

readc :: Read a => Char -> a
readc = read . (: [])

pG :: String -> G
pG s =
    let xs = zip [0 ..] $ lines s
        yss = map (second $ zip [0 ..] . map readc) xs
        f (x, ys) = map (unassoc . (x,)) ys
        g = mapFromList . map (second (\r -> V r 0 Nothing))
     in G{gg = g $ foldMap f yss, gsz = length xs}

neighbors (x, y) = do
    g <- gets (gg . stg)
    pure $
        mapMaybe
            (\c -> (c,) <$> mapLookup c g)
            [ (x2, y2)
            | x2 <- [x -1, x, x + 1]
            , y2 <- [y -1, y, y + 1]
            , x2 /= x || y2 /= y
            , x2 == x || y2 == y
            ]

-- Figuring this out took me just as long as it took pros to solve the whole
-- puzzle.
adjustStMap f c = modify (\s@(St{stg = g@(G{gg})}) -> s{stg = g{gg = mapAdjust f c gg}})

pathCost (V{vrisk, vscore}) = vrisk + vscore

calcScore c = do
    -- calculate my score
    --   look at my neighbors
    --   who has the lowest path cost?
    --   if it's lower than my current score,
    --      replace my cost and next location to use that neighbor
    this <- gets ((! c) . gg . stg)
    ns1 <- filter (isJust . vnext . snd) <$> neighbors c
    case ns1 of
        -- We're at the end already
        [] -> adjustStMap (\v -> v{vnext = Just c}) c
        ns -> do
            let best = minimumBy (compare `on` (pathCost . snd)) ns
                bestScore = pathCost (snd best)
            if bestScore < vscore this
                then adjustStMap (\v -> v{vscore = bestScore, vnext = Just (fst best)}) c
                else pure ()
            pure ()

walk c = do
    calcScore c
    -- find neighbors who can be updated
    --   look at my neighbors
    --   if they came through me, would they lower their cost?
    this <- gets ((! c) . gg . stg)
    ns <- neighbors c
    let upds =
            [ n
            | n <- ns
            , vscore (snd n) > pathCost this
                || vnext (snd n) == Nothing
            ]
    -- update them
    --   lower their cost and set their next location to me
    traverse_ (adjustStMap (\v -> v{vscore = pathCost this, vnext = Just c}) . fst) upds
    -- process them
    traverse_ (walk . fst) upds

shortPath s = vscore $ (! (0, 0)) $ gg $ stg $ execState (walk (gsz -1, gsz -1)) (St g)
  where
    g@G{gsz} = pG s

ans1 :: _
ans1 = shortPath dat

ans2 :: _
ans2 = undefined

-- ghcid needs this?
main = undefined
