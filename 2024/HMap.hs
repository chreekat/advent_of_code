module HMap (module HMap) where

import Data.HashMap.Lazy as HMap
import Data.Maybe qualified as Maybe

find f = Maybe.listToMaybe . findAll f

findAll f a = [i | (i, e) <- HMap.toList a, f e]

map_ // assocs = HMap.fromList assocs <> map_
