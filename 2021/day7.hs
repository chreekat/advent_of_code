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
    in minimum $ map (cost xs) [0..maximum xs]

ans1 :: _
ans1 = undefined

ans2 :: _
ans2 = undefined

-- ghcid needs this?
main = undefined
