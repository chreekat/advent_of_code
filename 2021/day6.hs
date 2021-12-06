{-# LANGUAGE PartialTypeSignatures #-}

import Tropes

ex1 = "3,4,3,1,2"

dat :: String
dat = unsafePerformIO (readFile "day6.txt")

pGroups :: String -> Map Integer Integer
pGroups = mapFromList . map (head &&& genericLength) . group . sort . map read . splitOn ","

dupFish timer ct =
    let t = timer - 1
     in if t < 0
            then [(6, ct), (8, ct)]
            else [(t, ct)]

stepDay :: Map Integer Integer -> Map Integer Integer
stepDay = fromListWith (+) . foldMap (uncurry dupFish) . mapToList

day d = last . take (d + 1) . iterate' stepDay . pGroups

dayCt d = sum . day d

ans1 :: _
ans1 = undefined

ans2 :: _
ans2 = undefined

-- ghcid needs this?
main = undefined
