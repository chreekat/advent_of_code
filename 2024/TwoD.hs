{-# OPTIONS_GHC -Wno-deprecations #-}
module TwoD where
import Data.Array (Array)
import qualified Data.Array as Array
import Control.Arrow
import Data.Maybe
import Map qualified

type TwoD a = Array (Int, Int) a

twoD' :: (Char -> a) -> String -> TwoD a
twoD' f s =
    let ls = lines s
        h = length ls
        w = length (head ls)
     in Array.listArray ((0, 0), (h - 1, w - 1)) (map f (concat ls))

twoD = twoD' id

showTwoD = showTwoD' id

showTwoD' :: (a -> Char) -> TwoD a -> String
showTwoD' f g =
    let ((x1, y1), (x2, y2)) = Array.bounds g
     in unlines
            [ [ f (g Array.! (x, y)) | y <- [y1 .. y2] ]
            | x <- [x1 .. x2]
            ]

showTwoD'' :: (a -> String) -> TwoD a -> String
showTwoD'' f g =
    let ((x1, y1), (x2, y2)) = Array.bounds g
     in unlines
            [ concat [ f (g Array.! (x, y)) | y <- [y1 .. y2] ]
            | x <- [x1 .. x2]
            ]

-- | filter the list of indices to those that are in the bounds of the array
filterBounds :: Array (Int, Int) a -> [(Int, Int)] -> [(Int, Int)]
filterBounds a = filter (Array.inRange (Array.bounds a))

listOfListOfIndices a = [[(x, y) | y <- [y1 .. y2]] | x <- [x1 .. x2]]
  where
    ((x1, y1), (x2, y2)) = Array.bounds a

inGrid g = Array.inRange (Array.bounds g)

insertRow :: Show a => Int -> [a] -> TwoD a -> Maybe (TwoD a)
insertRow x ys g | inGrid g (x,0) =
    let (x2, y2) = snd $ Array.bounds g
        top = Array.assocs $ Array.ixmap ((0,0), (x-1, y2)) id g
        bottom = Array.assocs $ Array.ixmap ((x+1,0), (x2+1, y2)) (first pred) g
        middlIndexes = [(x, j) | j <- [0 .. y2]]
    in Just $ Array.array ((0,0), (x2 + 1, y2)) (top ++ zip middlIndexes ys ++ bottom)
    | otherwise = Nothing

insertRow' a b c = fromJust $ insertRow a b c

insertCol :: Show a => Int -> [a] -> TwoD a -> Maybe (TwoD a)
insertCol y xs g | inGrid g (y, 0) =
    let (x2, y2) = snd $ Array.bounds g
        left = Array.assocs $ Array.ixmap ((0,0), (x2, y-1)) id g
        right = Array.assocs $ Array.ixmap ((0,y+1), (x2, y2+1)) (second pred) g
        middlIndexes = [(i, y) | i <- [0 .. x2]]
    in Just $ Array.array ((0,0), (x2, y2 + 1)) (left ++ zip middlIndexes xs ++ right)
    | otherwise = Nothing

insertCol' a b c = fromJust $ insertCol a b c

distance :: (Int, Int) -> (Int, Int) -> Int
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

neighbors :: (Int,Int) -> TwoD a -> [(Int, Int)]
neighbors (x, y) g =
    filterBounds g
        [ (x + 1, y)
        , (x - 1, y)
        , (x, y + 1)
        , (x, y - 1)
        ]

move (x,y) (dx,dy) m =
    if Array.inRange (Array.bounds m) (x+dx, y+dy)
        then Just (x+dx, y+dy)
        else Nothing

west,east,south,north,koillis,kaakko,lounas,luode
    :: (Array.Ix a, Array.Ix b, Num a, Num b)
    => (a, b) -> Array (a, b) e -> Maybe (a, b)
west (x,y) = move (x,y) (0,-1)
east (x,y) = move (x,y) (0,1)
north (x,y) = move (x,y) (-1,0)
south (x,y) = move (x,y) (1,0)
-- finnish directions
koillis (x,y) = move (x,y) (-1,1)
kaakko (x,y) = move (x,y) (1,1)
lounas (x,y) = move (x,y) (1,-1)
luode (x,y) = move (x,y) (-1,-1)

mapNeighbors (x,y) m =
    let (max,_) = Map.findMax m
        (min,_) = Map.findMin m
    in filter (Array.inRange (min,max))
        [ (x + 1, y)
        , (x - 1, y)
        , (x, y + 1)
        , (x, y - 1)
        ]
