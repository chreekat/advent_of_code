module Map (module Q, module Map) where

import Data.Map as Q

deleteGet :: Ord k => k -> Map k a -> (Maybe a, Map k a)
deleteGet k m = (Q.lookup k m, Q.delete k m)

