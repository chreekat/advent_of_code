module Array (module A, module Array) where


import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence qualified as Seq
import Data.Vector qualified as Vector
import TwoD qualified


import Data.Array as A

lookup :: Ix i => i -> Array i e -> Maybe e
lookup i a = if inRange (bounds a) i then Just (a ! i) else Nothing

find :: Ix i => (e -> Bool) -> Array i e -> Maybe i
find f = Maybe.listToMaybe . findAll f

findAll :: Ix i => (e -> Bool) -> Array i e -> [i]
findAll f a = [i | (i, e) <- assocs a, f e]

inBounds g = inRange (bounds g)
