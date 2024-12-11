module Set (module S, module Set) where

import Data.Set as S

lookup :: Ord a => a -> Set a -> Maybe a
lookup e s = if member e s then Just e else Nothing
