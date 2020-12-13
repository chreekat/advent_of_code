-- | Strict Map-based arrays for game-of-life stuff.

{-# LANGUAGE DeriveFunctor #-} 

module Ary where

import Data.Function
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Ary i b a = Ary { rangeMax :: b, aryMap :: Map i a }
    deriving (Eq, Show, Functor)


indices i j = [ (x, y) | x <- [1..i], y <- [1..j] ]


render2Ary :: Show a => (a -> String) -> Ary (Int,Int) [Int] a -> String
render2Ary printCel
    = unlines
    . map (concat . map (printCel . snd))
    . groupBy ((==) `on` (fst . fst))
    . Map.toList
    . aryMap


-- | Lookup of out-of-bounds item could be improved to O(1), but yolo
(!?) :: Ord i => Ary i b a -> i -> Maybe a
(Ary _ m) !? i = m Map.!? i
