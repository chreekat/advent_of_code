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





diagonal (L (x1, y1) (x2, y2)) = x1 /= x2 && y1 /= y2

drawLine d l@(L (x1,y1) (x2,y2))
    | diagonal l = d
    | x1 == x2 = foldr (markDiagram x1) d (range y1 y2)
    | otherwise = foldr (markDiagram `flip` y1) d (range x1 x2)


markDiagram x y = mapAlter (Just . maybe 1 succ) (x,y)


drawDiagram = foldl' drawLine mapEmpty . pLines

dangerous = mapFilter (> 1)

--
-- PRETTY
--

ans1 :: _
ans1 = length $ dangerous $ drawDiagram dat

ans2 :: _
ans2 = undefined

-- ghcid needs this?
main = undefined
