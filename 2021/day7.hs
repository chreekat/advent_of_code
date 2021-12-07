{-# LANGUAGE PartialTypeSignatures #-}

import Tropes

ex1 = "16,1,2,0,4,2,7,1,2,14\n"

dat :: String
dat = unsafePerformIO (readFile "day7.txt")

initials :: String -> [Int]
initials = map read . splitOn "," . init

cost xs i = sum . map (abs . (i -)) $ xs

f1 s =
    let xs = initials s
     in cost xs (median xs)

crabCost x y =
    let z = abs (x - y)
     in (z + 1) * z `div` 2

cost2 xs i = sum . map (crabCost i) $ xs

f2 s =
    let xs = initials s
     in minimum $ map (cost2 xs) [0 .. maximum xs]

ans1 :: _
ans1 = f1 dat

ans2 :: _
ans2 = f2 dat

-- ghcid needs this?
main = undefined
