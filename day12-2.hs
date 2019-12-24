{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wno-partial-type-signatures #-}

--import Control.Arrow
import Data.List
import Data.Maybe
--import Data.Function
import Data.Foldable
--import Data.Map (Map)
-- import qualified Data.Set as S
--import qualified Data.Map as M
import qualified Data.Sequence as Seq

-- import Debug.Pretty.Simple
--import Text.Pretty.Simple

type I = Int

data M = M V V
    deriving (Eq, Show, Ord)

type V = [I]

main :: IO ()
main = putStrLn "" >> r [1,10] >> s 0 [1,10] >> ss [1,10]

r = putStrLn . printRun . run
s i moons =
    let
    runs = run moons
    leHonks = printMoon i runs
    moonPhase = map printPhases (moonPhases runs)

    in
    putStrLn
        (unlines
            (zipWith
                (\a b -> intercalate " | " [a,b])
                leHonks
                moonPhase))

ss moons =
    let runs = run moons
        leHonks = map (printMoon `flip` runs) [0 .. length moons - 1]
        moonPhase = map printPhases (moonPhases runs)
    in
    putStrLn
        (unlines
            (map
                (intercalate " | ")
                (zipWith (++)
                    (transpose leHonks)
                    (map (:[]) moonPhase))))

runworld world =
    let dimRuns = map run (transpose world)
        dimPrints = map (printMoon 0) dimRuns
        maxSteps = maximum (map length dimRuns)
    in dimPrints

printPhases :: [Maybe Char] -> String
printPhases = map (fromMaybe ' ')

moonPhases :: [([I],[I],[I])] -> [[Maybe Char]]
moonPhases ress =
    let
    allXs = map (\(_,xs,_) -> xs) ress
    allVs = map (\(vs,_,_) -> vs) ress
    origXs = head allXs
    origVs = head allVs
    checkStep stepXs stepVs =
        let
        checkV = zipWith compare origVs stepVs
        checkX = zipWith compare origXs stepXs
        ids = ['A','B'..]
        goodMoon i = case (checkV !! i, checkX !! i) of
            (EQ, EQ) -> Just (ids !! i)
            _ -> Nothing
        in
        map goodMoon [0 .. length stepXs - 1]
    in
    zipWith checkStep allXs allVs

printMoon :: I -> [([I],[I],[I])] -> [String]
printMoon moonIdx ress=
    let moonXs = map (\(_,b,_) -> b) ress
        minimumX = minimum (concat moonXs)
        maximumX = maximum (concat moonXs) + 1
        space = ' '
        moonShapes = ".,*o"
        initString = Seq.fromList (take (maximumX - minimumX) (repeat space))
        tweakString i unAdjustedM seqq =
            let m = unAdjustedM - minimumX
                newChar seq = case Seq.lookup m seq of
                    Just x
                        | x == space
                        -> if i == moonIdx then 'O' else moonShapes !! i
                        | otherwise -> 'X'
                    Nothing -> '?'
            in Seq.update m (newChar seqq) seqq
        printStep xs =
            let tweak = foldr1 (.) (zipWith tweakString [0..] xs)
            in toList (tweak initString)
        printedSteps = map (printStep . (\(_,x,_) -> x)) ress
    in printedSteps

printRun :: [([I],[I],[I])] -> String
printRun ress =
    let as = ['a'..]
        dummyXs = (\(_,x,_) -> x) (head ress)
        hdr = intercalate "  " hdrs
        hdrs =
            map
                (\h ->
                    intercalate
                        " "
                        (map
                            (padS maxDigits)
                            (zipWith (\c _x -> [h,c]) as dummyXs)))
                "VXG"
        printStep (vs,xs,gs)
            = intercalate "  "
            . map (intercalate " " . map (printI maxDigits))
            $ [vs,xs,gs]
        nums = length ress
        maxDigits =
            length
                $ show
                $ maximum
                $ map
                    (\(a,b,c) -> maximum (map abs (concat [a,b,c])))
                    ress
    in unlines ((hdr : map printStep ress) ++ [show nums])

printI x i = (if i < 0 then "" else " ") ++ pad x i
pad i x = take n (repeat ' ') <> show x where n = i - length (show (abs x))
padS i x = take n (repeat ' ') <> x where n = i - length x + 1

run :: [I] -> [([I],[I],[I])]
run origX =
    let origV = take (length origX) (repeat 0)
        blorp vs xs =
            let grav = gravity xs
                newVs = zipWith (+) vs grav
                newXs = zipWith (+) newVs xs
            in (newVs, newXs, gravity newXs)
                : if [newXs, newVs] == [origX,origV] then [] else blorp newVs newXs
    in (origV, origX, gravity origX) : blorp origV origX


gravity :: [I] -> [I]
gravity xs =
    let gravI x = sum $ map (blah . flip compare x) xs
        blah = \case
            LT -> (-1)
            EQ -> 0
            GT -> 1
    in map gravI xs

ex :: [[I]]
ex =
    [[-1,0,2]
    ,[2,-10,-7]
    ,[4,-8,8]
    ,[3,5,-1]
    ]

input :: [[I]]
input =
  [[1, 2, -9]
  ,[-1, -9, -4]
  ,[17, 6, 8]
  ,[12, 4, 2]
  ]

-- <x=-8, y=-10, z=0>
-- <x=5, y=5, z=10>
-- <x=2, y=-7, z=3>
-- <x=9, y=-8, z=-3>

ex2 :: [[I]]
ex2 =
    [[-8, -10, 0]
    ,[5, 5, 10]
    ,[2, -7, 3]
    ,[9, -8, -3]
    ]
