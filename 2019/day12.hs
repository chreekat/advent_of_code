{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-partial-type-signatures #-}

import Control.Arrow
--import Data.List
--import Data.Maybe
import Data.Function
--import Data.Foldable
import Data.Map (Map)
-- import qualified Data.Set as S
import qualified Data.Map as M
--import qualified Data.Sequence as Sq

--import Debug.Pretty.Simple
import Text.Pretty.Simple

type I = Int
type World = Map I M

data M = M V V
    deriving (Eq, Show, Ord)

data V = V I I I
    deriving (Eq, Show, Ord)

(\/) :: V -> V -> V
(V a b c) \/ (V d e f) = V (a + d) (b + e) (c + f)

zeroV :: V
zeroV = V 0 0 0

main :: IO ()
main = pPrint $ energy (honk input !! 1000)

absV :: V -> I
absV (V a b c) = sum (map abs [a,b,c])

energyM :: M -> I
energyM (M v w) = absV v * absV w

energy :: World -> I
energy = M.foldr (\m -> (energyM m +)) 0

-- <x=-1, y=0, z=2>
-- <x=2, y=-10, z=-7>
-- <x=4, y=-8, z=8>
-- <x=3, y=5, z=-1>


iToV :: [I] -> V
iToV [a,b,c] = V a b c
iToV _ = error "iToV"

iToStationary :: [I] -> M
iToStationary = (M `flip` zeroV) . iToV

iToWorld :: [[I]] -> World
iToWorld
    = M.fromList . zip [0..] . map iToStationary

dummy :: World
dummy = iToWorld
    [[-1,0,2]
    ,[2,-10,-7]
    ,[4,-8,8]
    ,[3,5,-1]
    ]


-- <x=1, y=2, z=-9>
-- <x=-1, y=-9, z=-4>
-- <x=17, y=6, z=8>
-- <x=12, y=4, z=2>


input :: World
input = iToWorld
  [[1, 2, -9]
  ,[-1, -9, -4]
  ,[17, 6, 8]
  ,[12, 4, 2]
  ]

planets :: [I]
planets = [0..3]

gravityPI :: World -> I -> I -> V
gravityPI w p i =
    let (pickDim, constr) = isos !! i
        M pos _ = w M.! p
        pd = pickDim pos
        [gleft, gright] = map (\op -> M.filter (\(M (pickDim -> d) _) -> d `op` pd) w) [(<),(>)]
        gravX = ((-) `on` M.size) gright gleft
        grav = constr gravX
    in grav

gravityP :: World -> I -> V
gravityP w p = foldr1 (\/) (map (gravityPI w p) dims)

dims :: [I]
dims = [0..2]

vecX, vecY, vecZ :: V -> I

vecX (V x _ _) = x
vecY (V _ y _) = y
vecZ (V _ _ z) = z

xVec, yVec, zVec :: I -> V
xVec x = V x 0 0
yVec y = V 0 y 0
zVec z = V 0 0 z

isos :: [((V -> I), (I -> V))]
isos =
    [(vecX, xVec)
    ,(vecY, yVec)
    ,(vecZ, zVec)
    ]

gravity :: World -> Map I V
gravity w = M.fromList $ map (id &&& gravityP w) planets

accelerate :: World -> World
accelerate w = M.intersectionWith (\(M p v) v' -> M p (v \/ v')) w (gravity w)

move :: World -> World
move = M.map (\(M p v) -> M (p \/ v) v)

step :: World -> World
step = move . accelerate

honk :: World -> [World]
honk = iterate step 
