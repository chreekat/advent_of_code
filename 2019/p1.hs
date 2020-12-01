{-# LANGUAGE TypeApplications #-}

massFuel
    :: Int -- ^ Mass
    -> Int -- ^ Fuel
massFuel mass = blah !! mass
    where blah = 0 : 0 : 0 : map go [3..]
          go n = step n + (blah !! step n)
          step n = max 0 (n `div` 3 - 2)
