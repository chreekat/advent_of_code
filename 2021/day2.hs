{-# LANGUAGE PartialTypeSignatures #-}
import Tropes

ex1 =
    ["forward 5"
    ,"down 5"
    ,"forward 8"
    ,"up 3"
    ,"down 8"
    ,"forward 2"
    ]

dat :: String
dat = unsafePerformIO (readFile "day2.txt")

step1 (h,d) instr =
    let [dir, sval] = words instr
        val :: Int
        val = read sval
        h' = case dir of
            "forward" -> h + val
            _ -> h
        d' = case dir of
            "down" -> d + val
            "up" -> d - val
            _ -> d
    in (h',d')


ans1 :: Int
ans1 = prod2 (foldl' step1 (0,0) (lines dat))

step2 (h,d,a) instr =
    let [dir, sval] = words instr
        val :: Int
        val = read sval
        a' = case dir of
            "down" -> a + val
            "up" -> a - val
            _ -> a
        h' = case dir of
            "forward" -> h + val
            _ -> h
        d' = case dir of
            "forward" -> d + a * val
            _ -> d
    in (h',d',a')

ans2 :: Int
ans2 = prod2 (_3to2 (foldl' step2 (0,0,0) (lines dat)))

-- ghcid needs this?
main = undefined
