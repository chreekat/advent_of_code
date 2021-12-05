{-# LANGUAGE PartialTypeSignatures #-}

import Tropes


ex1, dat :: String
dat = unsafePerformIO (readFile "day5.txt")
ex1 = unsafePerformIO (readFile "ex5-1.txt")


data L = L (Int,Int) (Int,Int)
    deriving (Eq, Show)

pLine s =
    let [begin,_,end] = words s
        [[x1,y1],[x2,y2]] = map (map read . splitOn ",") [begin,end]
    in L (x1,y1) (x2,y2)

pLines = map pLine . lines



lineRange (L (x1,y1) (x2,y2)) = zip (range x1 x2) (range y1 y2)


diagonal (L (x1, y1) (x2, y2)) = x1 /= x2 && y1 /= y2

drawLine d l@(L (x1,y1) (x2,y2))
    | diagonal l = d
    | x1 == x2 = foldr (markDiagram x1) d (range y1 y2)
    | otherwise = foldr (markDiagram `flip` y1) d (range x1 x2)


withDiag d l = mapUnionsWith (+) (d : map (mapSingleton `flip` 1) (lineRange l))

drawLine' f d l@(L (x1,y1) (x2,y2))
    | diagonal l = f d l
    | x1 == x2 = foldr (markDiagram x1) d (range y1 y2)
    | otherwise = foldr (markDiagram `flip` y1) d (range x1 x2)


markDiagram x y = mapAlter (Just . maybe 1 succ) (x,y)


drawDiagram = foldl' drawLine mapEmpty . pLines

drawDiagramWithDiag = foldl' (drawLine' withDiag) mapEmpty . pLines

dangerous = mapFilter (> 1)

--
-- PRETTY
--

ans1 :: _
ans1 = length $ dangerous $ drawDiagram dat

ans2 :: _
ans2 = length $ dangerous $ drawDiagramWithDiag dat

-- ghcid needs this?
main = undefined
