{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State as State
import Data.Bifunctor
import Data.Foldable
import Data.List.Split
import Data.Maybe
import Data.Monoid
import System.IO.Unsafe
import qualified Data.Vector as V

import SimpleParse

test = unsafePerformIO (readFile "day12-test.txt")
input = unsafePerformIO (readFile "day12-input.txt")

main = do
    print part1test
    print part1actual
    print part2test
    print part2actual

part1test = part1 test
part1actual = part1 input
part2test = "hi"
part2actual = "hi"

data IT = F | L | R | N | E | S | W
    deriving (Eq, Show, Read)

data Hdg = NN | EE |  SS | WW
    deriving (Eq, Show, Enum)

data Instr = Instr IT Int
    deriving (Eq, Show)

pInstr (c:cs) = Instr (read [c]) (read cs)

pInstrs = map pInstr . lines

data NavDat = NavDat { lat :: Int, lon :: Int, hdg ::  Hdg }
    deriving (Eq, Show)

toLoc (NavDat la lo _) = Loc la lo

step1 (Instr F z) = do
    NavDat la lo hd <- get
    case hd of
        NN -> goN z
        EE -> goE z
        SS -> goS z
        WW -> goW z

step1 (Instr R z) = goCW (z `div` 90)
step1 (Instr L z) = goCW ((-1) * (z `div` 90))

step1 (Instr N z) = goN z
step1 (Instr E z) = goE z
step1 (Instr S z) = goS z
step1 (Instr W z) = goW z

goN z = modify (\x -> x { lat = lat x + z })
goS z = modify (\x -> x { lat = lat x - z })
goE z = modify (\x -> x { lon = lon x + z })
goW z = modify (\x -> x { lon = lon x - z })

goCW z = modify (\x -> x { hdg = toEnum ((fromEnum (hdg x) + z) `mod` 4) })

-- mkState :: _ -> String -> State.State NavDat ()
mkState s = traverse_ s . pInstrs

initNa = NavDat 0 0 EE

part1 = manhattan step1 toLoc initNa

manhattan :: (Instr -> State.State s ()) -> (s -> Loc) -> s -> String -> Int
manhattan s f ini inp =
    let Loc la lo = f (execState (mkState s inp) ini)
    in abs la + abs lo

data Loc = Loc { llat :: Int, llon :: Int }
    deriving (Eq, Show)

data FullNavDat = FullNavDat { shipPos :: Loc, wpPos :: Loc }
    deriving (Eq, Show)



initFulNa = FullNavDat (Loc 0 0) (Loc 1 10)

step2 (Instr R z) = rotBearingCW (z `div` 90)
step2 (Instr L z) = rotBearingCW ((-1) * (z `div` 90))
step2 (Instr N z) = txBearingN z
step2 (Instr E z) = txBearingE z
step2 (Instr S z) = txBearingS z
step2 (Instr W z) = txBearingW z

step2 (Instr F z) = do
    FullNavDat (Loc la lo) wy@(Loc wla wlo) <- get
    put (FullNavDat (Loc (la + wla * z) (lo + wlo * z)) wy)

rotBearingCW z = do
    FullNavDat loc (Loc la lo) <- get
    let (la', lo') =
            case toEnum (z `mod` 4) of
                NN -> (la, lo)
                EE -> (-lo, la)
                SS -> (-la, -lo)
                WW -> (lo, -la)
    put (FullNavDat loc (Loc la' lo'))

txBearingN z = do
    FullNavDat loc (Loc la lo) <- get
    put (FullNavDat loc (Loc (la + z) lo))

txBearingS z = do
    FullNavDat loc (Loc la lo) <- get
    put (FullNavDat loc (Loc (la - z) lo))

txBearingE z = do
    FullNavDat loc (Loc la lo) <- get
    put (FullNavDat loc (Loc la (lo + z)))

txBearingW z = do
    FullNavDat loc (Loc la lo) <- get
    put (FullNavDat loc (Loc la (lo - z)))

part2 = manhattan step2 shipPos initFulNa
