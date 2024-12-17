{-# LANGUAGE ViewPatterns #-}
module Seq (module S, module Seq) where

import Data.Sequence as S
import Data.Sequence (Seq ((:<|), (:|>)), Seq (Empty))

data Zip a = Zip {left :: Seq a, focus :: a, right :: Seq a}
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

rzip (rest :|> a) = Just $ Zip rest a Empty
rzip Empty = Nothing

lzip (a :<| rest) = Just $ Zip Empty a rest
lzip Empty = Nothing

-- | If the index points to the last element, you'll get the equivalent of rzip
--  If it points to the first (zeroth) element, you'll get the equivalent of
--  lzip
toZip :: Word -> Seq a -> Maybe (Zip a)
toZip (fromIntegral -> i) s 
    | i > Prelude.length s = Nothing
    | i == 0 = lzip s
    | Empty <- s = Nothing
    | otherwise =
        let (as:|>a,back) = S.splitAt i s
        in Just $ Zip as a back


goLeft :: Zip a -> Zip a
goLeft (Zip (l :|> a) f r) = Zip l a (f :<| r)
goLeft z = z

goRight :: Zip a -> Zip a
goRight (Zip l f (r :<| a)) = Zip (l :|> f) r a
goRight z = z
