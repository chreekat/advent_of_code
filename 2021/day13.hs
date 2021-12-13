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


data Inp = Inp { inpInstrs :: [Instr], inpGrid :: Grid
    } deriving (Eq, Show)

data Grid = Grid { maxX :: Max Int, maxY :: Max Int, gridMap :: Set Coord 
    } deriving (Eq, Show)


instance Semigroup Grid where
    Grid x y s <> Grid x1 y1 s1 =
        Grid (x <> x1) (y <> y1) (s <> s1)

instance Monoid Grid where
    mempty = Grid (Max 0) (Max 0) (setEmpty)

type Coord = (Int, Int)

type Instr = Either Int Int


pInput str =
    let [s1,s2] = splitOn [""] $ lines str
        pCoord s = let [x,y] = map read $ splitOn "," s
                 in (coerce Grid) x y (setSingleton (x,y))
        pInstr s = let [dir,amt] = splitOn "=" $ head $ drop 2 $ words s
                 in (case dir of
                        "x" -> Left
                        "y" -> Right)
                        (read amt)
    in Inp (map pInstr s2) (foldMap pCoord s1)



ans1 :: _
ans1 = undefined

ans2 :: _
ans2 = undefined

-- ghcid needs this?
main = undefined
