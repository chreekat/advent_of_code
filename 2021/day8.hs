{-# LANGUAGE PartialTypeSignatures #-}

import Tropes

ex1, dat :: String

ex0 = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf\n"

ex1 =
    unlines
        [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
        , "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
        , "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
        , "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
        , "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
        , "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
        , "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
        , "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
        , "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
        , "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
        ]

dat = unsafePerformIO (readFile "day8.txt")

pEntry s =
    let [obs, out] = splitOn "|" s
        obss = words obs
        outs = words out
     in (obss, outs)

pEntries = map pEntry . lines

numSimpleOut = sum . map (length . filter (`elem` [2, 4, 3, 7]) . map length . snd) . pEntries

ans1 :: _
ans1 = numSimpleOut dat

-- a is in 7,8
-- b is in 4,8
-- c is in 1,4,7,8
-- d is in 4,8
-- e is in 8
-- f is in 1,4,7,8

-- 1 is     c,    f
-- 4 is   b,c,d,  f
-- 7 is a,  c,    f
-- 8 is a,b,c,d,e,f,g
--
-- 0 is a,b,c,  e,f,g
-- 2 is a,  c,d,e,  g
-- 3 is a,  c,d,  f,g
-- 5 is a,b,  d,  f,g
-- 6 is a,b,  d,e,f,g
-- 9 is a,b,c,d,  f,g
--
-- 1,7 determine a! and c,f
-- 1,4 determine b,d and c,f
-- 4,8 determine a,e,g and b,c,d,f and a,e,g
-- 7,8 deterimen b,d,e,g
-- 4,7,8 deterimen e,g and a,b,c,d,f
-- 4,7 determine a and c,f and b,d
-- 0,6 deterimen c,d and a,b,e,f,g
-- 2,3,5 deterimine b

-- Some components are uniquely determined by how many times they show up.
-- e = 4  !!
-- b = 6  !!
-- d,g = 7
-- a,c = 8
-- f = 9  !!

-- Back to the beginning, some numbers are uniquely determined by number of
-- components.
--
-- _1 = 2
-- _7 = 3
-- _4 = 4
-- _2,_3,_5 = 5
-- _0,_6,_9 = 6
-- _8 = 7

get1748 :: [String] -> [String]
get1748 = sortBy (compare `on` length) . filter ((`elem` [2, 3, 4, 7]) . length)

getBEFaC allPats =
    let comps = "abcdefg"
        freqs = map (\a -> length (filter (a `elem`) allPats)) comps
        Just b = fmap (comps !!) (findIndex (== 6) freqs)
        Just e = fmap (comps !!) (findIndex (== 4) freqs)
        Just f = fmap (comps !!) (findIndex (== 9) freqs)
        ac = fmap (comps !!) (findIndices (== 8) freqs)
     in (b, e, f, ac)

getA one seven = head $ seven \\ one

getD b one four = head $ four \\ one <> [b]

getC a ac = head $ ac \\ [a]

findMapping allPats =
    let [one, seven, four, eight] = get1748 allPats
        (b, e, f, ac) = getBEFaC allPats
        a = getA one seven
        d = getD b one four
        c = getC a ac
        g = head $ "abcdefg" \\ [a, b, c, d, e, f]
        zero = [a, b, c, e, f, g]
        two = [a, c, d, e, g]
        three = [a, c, d, f, g]
        five = [a, b, d, f, g]
        six = [a, b, d, e, f, g]
        nine = [a, b, c, d, f, g]
     in ( [a, b, c, d, e, f, g]
        , [zero, one, two, three, four, five, six, seven, eight, nine]
        )

digit :: [String] -> String -> Int
digit nums d = fromJust $ findIndex (((==) `on` sort) d) nums

val :: [String] -> [String] -> Int
val nums digits =
    let ds = map (digit nums) digits
     in sum (zipWith (*) (iterate (* 10) 1) (reverse ds))

values s =
    let (allPats, outs) = pEntry s
        (maps, nums) = findMapping allPats
     in val nums outs

ans2 :: _
ans2 = sum $ map values (lines dat)

-- ghcid needs this?
main = undefined
