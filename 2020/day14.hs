{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Data.Bool
import Data.Bits
import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Identity
import System.IO.Unsafe
import qualified Data.IntMap as IntMap
import qualified Data.Vector as V

import SimpleParse hiding (State)

test = unsafePerformIO (readFile "day14-test.txt")
input = unsafePerformIO (readFile "day14-input.txt")

main = do
    print part1test
    print part1actual
    print part2test
    print part2actual

part1test = "hi"
part1actual = "hi"
part2test = "hi"
part2actual = "hi"

type Mask = (Word, Word)
type Write = (Int, Word)

data Input = IMask Mask | IW Write
    deriving (Eq, Show)

applyMask :: Mask -> Word -> Word
applyMask (ones, zeros) i = (i .|. ones) .&. complement zeros

renderBits :: Word -> String
renderBits i = "0b" <> reverse (go i)
    where go i = show (i .&. 1) <> (if i > 1 then go (shiftR i 1) else "" )

pInput s = map go (lines s) where
    go l =
        let [cmd, _, val] = words l
        in case cmd of
            "mask" -> IMask (pMask val)
            _ -> IW (pAddr cmd, read val)


pMask s = 
    let bits = zip [0..] (reverse s)
        oneBits = map fst $ filter ((== '1') . snd) bits
        zeroBits = map fst $ filter ((== '0') . snd) bits
        ones = foldl' setBit 0 oneBits
        zeros = foldl' setBit 0 zeroBits
   in (ones, zeros) 

pAddr :: String -> Int
pAddr = read . init . drop 4

step :: Input -> State (Mask, IntMap.IntMap Word) ()
step (IMask m) = modify (first (const m))
step (IW (addr, val)) = do
    m <- gets fst
    modify (second (IntMap.insert addr (applyMask m val)))

fresh :: (Mask, IntMap.IntMap Word)
fresh = ((0,0) , IntMap.empty)

bleh s = foldMap Sum $ snd $ execState (traverse_ step (pInput s)) fresh

uf w x i =
    if testBit x i
    then [0,1]
    else [bool 0 1 (testBit (w .&. bit i) i)]

addrs :: Word -> Word -> [Word]
addrs w x =
    let sigBits w = finiteBitSize w - countLeadingZeros w - 1
        mostBits = max (sigBits w) (sigBits x)

        asWord :: [Int] -> Word
        asWord = foldl' setBit 0 . map fst . filter ((==1) . snd) . zip [0..]
    in sort
        . map asWord
        . sequence
        . map (uf w x)
        $ [0 .. mostBits]
