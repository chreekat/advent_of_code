{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-unused-top-binds #-}

import Control.Arrow
import Data.List
import Data.Function

data Image = Image
    { iwidth :: Int
    , iheight :: Int
    , bits :: [Int]
    } deriving (Eq, Show)

data Pixel = B | W | T
    deriving (Eq, Show)

decodePixel :: Int -> Pixel
decodePixel = ([B, W, T] !!)

instance Semigroup Pixel where
    T <> p = p
    p <> _ = p

instance Monoid Pixel where
    mempty = T

data Layer = Layer
    { lwidth :: Int
    , lheight :: Int
    , lPxs :: [Pixel]
    } deriving (Eq, Show)

instance Semigroup Layer where
    Layer a b px1 <> Layer _ _ px2 = Layer a b (zipWith (<>) px1 px2)

layers :: Image -> [Layer]
layers (Image w h bbs) = unfoldr stripLayer bbs
    where
        stripLayer [] = Nothing
        stripLayer bs = Just $ first (Layer w h . map decodePixel) $ splitAt (w * h) bs

getLayer :: Int -> Image -> Layer
getLayer i = (!! i) . layers

readImage :: Int -> Int -> String -> Image
readImage w h = Image w h . map (read @ Int) . map (:[]) . head . lines

dummy = "123456789012"

check :: Image -> Int
check img =
    let layer = minimumBy (compare `on` length . filter (==B) . lPxs) (layers img)
    in product (map (length . flip filter (lPxs layer) . (==)) [W,T])

main = do
    input <- readFile "day8-input"
    part1 input
    part2 input

part1 = print . check . readImage 25 6
part2 = putStrLn . printLayer . foldl1 (<>) . layers . readImage 25 6

printPx W = '.'
printPx B = 'X'
printPx T = ' '

printLayer (Layer w _ pxs) = unlines . chunksOf w . map printPx $ pxs

chunksOf n = unfoldr takeChunk
  where
    takeChunk [] = Nothing
    takeChunk bs = Just $ splitAt n bs
