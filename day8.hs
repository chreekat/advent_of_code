{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-unused-top-binds #-}

import Data.List
import Data.Function

data Image = Image
    { width :: Int
    , height :: Int
    , bits :: [Int]
    } deriving (Eq, Show)

type Layer = [Int]

layers :: Image -> [Layer]
layers (Image w h bbs) = unfoldr stripLayer bbs
    where
        stripLayer [] = Nothing
        stripLayer bs = Just $ splitAt (w * h) bs

getLayer :: Int -> Image -> Layer
getLayer i = (!! i) . layers

readImage :: Int -> Int -> String -> Image
readImage w h = Image w h . map (read @ Int) . map (:[]) . head . lines

dummy = "123456789012"

check :: Image -> Int
check img =
    let layer = minimumBy (compare `on` length . filter (==0)) (layers img)
    in product (map (length . flip filter layer . (==)) [1,2])

main = print =<< check . readImage 25 6 <$> readFile "day8-input"
