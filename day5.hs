{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-unused-top-binds #-}
import Control.Monad.Free
import Data.Foldable
import Data.Function
import Data.Map (Map)
import Data.Monoid
import GHC.Generics
import Test.QuickCheck
import qualified Data.Map as M

-- | Buts
data CrankF f
    = FiddleBits Memory f
    | ReadIO f
    | WriteIO f
    | Stop
    deriving (Functor)

type Crank = Free CrankF

fiddleBits :: Memory -> Crank ()
fiddleBits c = liftF (FiddleBits c ())

readd :: Crank ()
readd = liftF (ReadIO ())

writeIO :: Crank ()
writeIO = liftF (WriteIO ())

stop :: Crank ()
stop = liftF Stop

data Cmd = Add | Mul | Input | Output | Halt
    deriving (Eq, Show)

-- | Would be hidden in a real program
data Opcode = Op Cmd Int

data ParamMode = Pos | Imm
    deriving (Eq, Show)

mode :: Int -> ParamMode
mode 0 = Pos
mode 1 = Imm
mode _ = error "HCF"

-- | Source of troof
opcodes :: Map Int Opcode
opcodes = M.fromList [(1, Op Add 3), (2, Op Mul 3), (3, Op Input 2), (4, Op Output 1), (99, Op Halt 0)]

readOpcode :: Int -> Maybe (Cmd, [ParamMode])
readOpcode code = do
    Op cmd paramCt <- M.lookup (code `mod` 100) opcodes
    let modes = map mode . map (\i -> code `div` 10^i `mod` 10) $ take paramCt ([2..] :: [Int])
    pure (cmd, modes)

type Ram = Map Int Int
type PC = Int

data Memory = Memory 
    { memRam :: Ram
    , memPC :: PC
    } deriving (Eq, Show, Generic)

instance Arbitrary Memory where
    arbitrary = Memory <$> arbitrary <*> arbitrary
    shrink = genericShrink

data Status = Running | Halted | Error
    deriving (Eq, Show, Generic)

instance Arbitrary Status where
    arbitrary = elements [Running, Halted, Error]
    shrink = genericShrink

data Computer = Computer
    { computerStatus :: Status
    , computerMemory :: Memory
    } deriving (Eq, Show, Generic)

instance Arbitrary Computer where
    arbitrary = Computer <$> arbitrary <*> arbitrary
    shrink = genericShrink

evalStep :: Memory -> Either Computer Computer
evalStep mem@(Memory ram pc) = case (readOpcode =<< M.lookup pc ram) of
    Just (Add, _modes) -> Right $ Computer Running (add mem)
    Just (Mul, _modes) -> Right $ Computer Running (mul mem)
    -- Just (Input, _modes) -> Right $ Computer Running (inp mem)
    -- Just (Output, _modes) -> Right $ Computer Running (out mem)
    Just (Halt, _) -> Left $ Computer Halted mem
    Nothing -> Left $ Computer Error mem 

evalStep' :: Memory -> Either Computer (Crank ())
evalStep' mem@(Memory ram pc) = case (readOpcode =<< M.lookup pc ram) of
    Just (Add, _modes) -> Right $ fiddleBits $ add mem
    Just (Mul, _modes) -> Right $ fiddleBits $ mul mem
    Just (Input, _modes) -> Right readd
    Just (Output, _modes) -> Right writeIO
    Just (Halt, _) -> Right stop
    Nothing -> Left $ Computer Error mem 

add, mul :: Memory -> Memory
add (Memory ram pc) =
    let [a1p,a2p,cp] = map (ram M.!) [pc+1,pc+2,pc+3]
        [a1,a2] = map (ram M.!) [a1p,a2p]
    in Memory (M.insert cp (a1 + a2) ram) (pc + 4)


mul (Memory ram pc) =
    let [a1p,a2p,cp] = map (ram M.!) [pc+1,pc+2,pc+3]
        [a1,a2] = map (ram M.!) [a1p,a2p]
    in Memory (M.insert cp (a1 * a2) ram) (pc + 4)

inp :: Int -> Memory -> Memory
inp = undefined

out :: Memory -> Int
out = undefined

evalProgram :: [Int] -> [Int]
evalProgram input =
    let mem0 = Memory (M.fromList (zip [0..] input)) 0
        step _   (Left c) = c
        step nxt (Right (Computer _ mem)) = nxt (evalStep mem)
    in M.elems $ memRam $ computerMemory $ fix step (Right (Computer Running mem0))

evalProgram' :: [Int] -> [Int]
evalProgram' input =
    let mem0 = Memory (M.fromList (zip [0..] input)) 0
        step _   (Left c) = c
        step nxt (Right (Computer _ mem)) = nxt (evalStep' mem)
    in M.elems $ memRam $ computerMemory $ fix step (Right (Computer Running mem0))



honk = [3,225,1,225,6,6,1100,1,238,225,104,0,1002,36,25,224,1001,224,-2100,224,4,224,1002,223,8,223,101,1,224,224,1,223,224,223,1102,31,84,225,1102,29,77,225,1,176,188,224,101,-42,224,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,2,196,183,224,1001,224,-990,224,4,224,1002,223,8,223,101,7,224,224,1,224,223,223,102,14,40,224,101,-1078,224,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1001,180,64,224,101,-128,224,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,1102,24,17,224,1001,224,-408,224,4,224,1002,223,8,223,101,2,224,224,1,223,224,223,1101,9,66,224,1001,224,-75,224,4,224,1002,223,8,223,1001,224,6,224,1,223,224,223,1102,18,33,225,1101,57,64,225,1102,45,11,225,1101,45,9,225,1101,11,34,225,1102,59,22,225,101,89,191,224,1001,224,-100,224,4,224,1002,223,8,223,1001,224,1,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,226,677,224,1002,223,2,223,1006,224,329,1001,223,1,223,108,226,226,224,1002,223,2,223,1006,224,344,1001,223,1,223,7,677,226,224,102,2,223,223,1005,224,359,101,1,223,223,7,226,677,224,102,2,223,223,1006,224,374,101,1,223,223,1008,677,226,224,1002,223,2,223,1006,224,389,101,1,223,223,8,677,677,224,1002,223,2,223,1005,224,404,101,1,223,223,8,677,226,224,102,2,223,223,1005,224,419,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,434,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,449,1001,223,1,223,107,677,226,224,1002,223,2,223,1005,224,464,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,479,1001,223,1,223,1108,677,226,224,1002,223,2,223,1006,224,494,1001,223,1,223,1108,677,677,224,1002,223,2,223,1006,224,509,1001,223,1,223,107,677,677,224,1002,223,2,223,1005,224,524,101,1,223,223,1007,677,226,224,102,2,223,223,1005,224,539,1001,223,1,223,1107,226,226,224,1002,223,2,223,1006,224,554,1001,223,1,223,1008,226,226,224,1002,223,2,223,1006,224,569,101,1,223,223,1108,226,677,224,1002,223,2,223,1006,224,584,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,1007,677,677,224,102,2,223,223,1006,224,614,101,1,223,223,107,226,226,224,102,2,223,223,1006,224,629,101,1,223,223,1007,226,226,224,102,2,223,223,1005,224,644,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,659,1001,223,1,223,7,677,677,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226]

runInput noun verb =
    let (x:_:_:inp') = honk
    in head $ evalProgram (x:noun:verb:inp')

output19690720 :: First (Int,Int)
output19690720 = fold $ [ bonk x y | x <- [0..99], y <- [0..99] ]
    where bonk x y = case runInput x y of 
            19690720 -> First (Just (x,y))
            _ -> First Nothing

main = 
    let First (Just (n,v)) = output19690720
    in print $ 100 * n + v



isRight (Right _) = True
isRight _ = False

newtype Sensible = Sensible Memory
    deriving (Eq, Show, Generic)

instance Arbitrary Sensible where
    arbitrary = do
        codes <- listOf $ elements (M.keys opcodes)
        filler <- vectorOf (length codes * 3) (getNonNegative <$> arbitrary)
        let (_,input) = foldr (\c (f1:f2:f3:fill,shtuff) -> (fill, c:f1:f2:f3:shtuff)) (filler,[]) codes
        pure $ Sensible $ Memory (M.fromList (zip [0..] input)) 0
