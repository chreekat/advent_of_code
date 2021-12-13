{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Tropes

ex1, dat :: String
dat = unsafePerformIO (readFile "day13.txt")
ex1 = unsafePerformIO (readFile "day13-ex1.txt")

data Inp = Inp
    { inpInstrs :: [Instr]
    , inpGrid :: Grid
    }
    deriving (Eq, Show)

data Grid = Grid
    { maxX :: Max Int
    , maxY :: Max Int
    , gridMap :: Set Coord
    }
    deriving (Eq, Show)

instance Semigroup Grid where
    Grid x y s <> Grid x1 y1 s1 =
        Grid (x <> x1) (y <> y1) (s <> s1)

instance Monoid Grid where
    mempty = Grid (Max 0) (Max 0) (setEmpty)

type Coord = (Int, Int)

type Instr = Either Int Int

pInput str =
    let [s1, s2] = splitOn [""] $ lines str
        pCoord s =
            let [x, y] = map read $ splitOn "," s
             in (coerce Grid) x y (setSingleton (x, y))
        pInstr s =
            let [dir, amt] = splitOn "=" $ head $ drop 2 $ words s
             in ( case dir of
                    "x" -> Left
                    "y" -> Right
                )
                    (read amt)
     in Inp (map pInstr s2) (foldMap pCoord s1)

foldGrid :: Grid -> Instr -> _
foldGrid gr ins
    | (Left x) <- ins =
        let f g = setUnion g . setMap (first ((x -) . (subtract x)))
         in gr{maxX = Max (x -1), gridMap = uncurry f (setPartition ((< x) . fst) (gridMap gr))}
    | (Right y) <- ins =
        let f g = setUnion g . setMap (second ((y -) . (subtract y)))
         in gr{maxY = Max (y -1), gridMap = uncurry f (setPartition ((< y) . snd) (gridMap gr))}

pGrid :: Grid -> IO ()
pGrid (Grid mx my g) =
    putStr $
        unlines
            [ pts
            | y <- [0 .. getMax my]
            , let xs = map (,y) [0 .. getMax mx]
            , let pts = map pt xs
            ]
  where
    pt c = if setMember c g then '#' else '.'

func1 s =
    let Inp{inpInstrs, inpGrid} = pInput s
     in setSize $ gridMap $ foldGrid inpGrid (head inpInstrs)

ans1 :: _
ans1 = func1 dat

func2 s =
    let Inp{inpInstrs, inpGrid} = pInput s
     in pGrid $ foldl' foldGrid inpGrid inpInstrs

ans2 :: _
ans2 = func2 dat

-- ghcid needs this?
main = undefined
