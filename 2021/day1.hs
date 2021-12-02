{-# LANGUAGE PartialTypeSignatures #-}
import Tropes

step Nothing i = Just (0, i)
step (Just (ct, i)) j = Just ((if i < j then 1 else 0) + ct, j) 


ex1 = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

puz1 :: [Int] -> _
puz1 = foldl' step Nothing

dat :: [Int]
dat = map read (lines (unsafePerformIO (readFile "day1.txt")))

ans1_1 = fst <$> (puz1 dat)

theSeq x = map sum3 (zip3 x (tail x) (tail (tail x)))


ans1_2 = fst <$> (puz1 (theSeq (dat)))

-- ghcid needs this?
main = undefined
