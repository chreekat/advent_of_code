{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-unused-top-binds #-}

import Data.Function
import Data.Map (Map)
import qualified Data.Map as M

data Cmd = Add | Mul | Input | Output | JumpT | JumpF | StoreLT | StoreEQ | Halt
    deriving Show

-- | Would be hidden in a real program
data OpCode a = Op Cmd a

deriving instance Show a => Show (OpCode a)

data ParamMode = Pos | Imm
    deriving (Eq, Show)

mode :: Int -> ParamMode
mode 0 = Pos
mode 1 = Imm
mode _ = error "HCF"

-- | Source of troof
opcodes :: Map Int (OpCode Int)
opcodes = M.fromList
    [(1, Op Add 3)
    ,(2, Op Mul 3)
    ,(3, Op Input 2)
    ,(4, Op Output 1)
    ,(5, Op JumpT 2)
    ,(6, Op JumpF 2)
    ,(7, Op StoreLT 3)
    ,(8, Op StoreEQ 3)
    ,(99, Op Halt 0)
    ]

readOpCode :: Int -> Maybe (OpCode [ParamMode])
readOpCode code = do
    Op cmd paramCt <- M.lookup (code `mod` 100) opcodes
    let modes = map mode . map (\i -> code `div` 10^i `mod` 10) $ take paramCt ([2..] :: [Int])
    pure (Op cmd modes)

-- | Cheating and using PC as a generic pointer
deref :: Memory -> ParamMode -> Int
deref (Memory ram pc) = \case
    Imm -> ram M.! pc
    Pos -> ram M.! (ram M.! pc)

-- | Relative offset for location of the ptr
relDeref :: Memory -> Int -> ParamMode -> Int
relDeref (Memory ram pc) rel = deref (Memory ram (pc+rel))

type Ram = Map Int Int
type PC = Int

data Memory = Memory
    { memRam :: Ram
    , memPC :: PC
    } deriving (Eq, Show)

data Status = Running | Halted | Error
    deriving (Eq, Show)

data Computer = Computer
    { computerStatus :: Status
    , computerMemory :: Memory
    } deriving (Eq, Show)

evalStep :: Memory -> IO Computer
evalStep mem@(Memory ram pc) = case readOpCode =<< M.lookup pc ram of
    Just (Op Add ps) -> pure $ Computer Running (add mem ps)
    Just (Op Mul ps) -> pure $ Computer Running (mul mem ps)
    Just (Op Input ps) -> Computer Running <$> inp mem ps
    Just (Op Output ps) -> Computer Running <$> out mem ps
    Just (Op JumpT ps) -> pure $ Computer Running (jmpt mem ps)
    Just (Op JumpF ps) -> pure $ Computer Running (jmpf mem ps)
    Just (Op StoreLT ps) -> pure $ Computer Running (stolt mem ps)
    Just (Op StoreEQ ps) -> pure $ Computer Running (stoeq mem ps)
    Just (Op Halt _) -> pure $ Computer Halted mem
    Nothing -> pure $ Computer Error mem

add, mul :: Memory -> [ParamMode] -> Memory
add = binOp (+)
mul = binOp (*)

binOp :: (Int -> Int -> Int) -> Memory -> [ParamMode] -> Memory
binOp op (Memory ram pc) ps =
    let [a1,a2] = zipWith (deref . Memory ram) [pc+1, pc+2] ps
        cp = ram M.! (pc + 3)
    in Memory (M.insert cp (a1 `op` a2) ram) (pc + 4)

inp :: Memory -> [ParamMode] -> IO Memory
inp (Memory ram pc) _ = do
    putStr "Input> "
    c <- getLine
    let cp = ram M.! (pc + 1)
    pure $ Memory (M.insert cp (read c) ram) (pc+2)

out :: Memory -> [ParamMode] -> IO Memory
out (Memory ram pc) [p] = (Memory ram (pc+2)) <$ print (deref (Memory ram (pc + 1)) p)
out _               _   = error "HCF in out"

jmpt, jmpf :: Memory -> [ParamMode] -> Memory
jmpt = jmpCond (/= 0)
jmpf = jmpCond (== 0)

jmpCond :: (Int -> Bool) -> Memory -> [ParamMode] -> Memory
jmpCond cond mem@(Memory ram pc) = \case
    (zipWith (relDeref mem) [1,2] -> [t,a]) ->
        Memory ram (if cond t then a else pc + 3)
    _ -> error "HCF in jmpCond"

stoeq, stolt :: Memory -> [ParamMode] -> Memory
stoeq = stoCmp (==)
stolt = stoCmp (<)

stoCmp :: (Int -> Int -> Bool) -> Memory -> [ParamMode] -> Memory
stoCmp cond mem@(Memory ram pc) =
    let cp = ram M.! (pc + 3)
    in \case
        (zipWith (relDeref mem) [1,2] -> [a1,a2]) ->
            Memory
                (M.insert cp (if cond a1 a2 then 1 else 0) ram)
                (pc + 4)
        _ -> error "HCF in stoCmp"

-- fix :: (a -> a) -> a
--     :: ((a -> b) -> (a -> b)) -> (a -> b)
--     :: ((a -> IO b) -> (a -> IO b)) -> a -> IO b
--
-- lol

isRunning :: Computer -> Bool
isRunning (Computer Running _) = True
isRunning _ = False

evalProgram :: [Int] -> IO [Int]
evalProgram input =
    let mem0 = Memory (M.fromList (zip [0..] input)) 0
        step nxt c
            | isRunning c = nxt =<< evalStep (computerMemory c)
            | otherwise = pure c
    in do
        c@(Computer resStat resRam) <- fix step (Computer Running mem0)
        case resStat of
            Halted -> pure (M.elems (memRam resRam)) -- elems gives ascending order
            _ -> error (show c)

honk :: [Int]
honk = [3,225,1,225,6,6,1100,1,238,225,104,0,1002,36,25,224,1001,224,-2100,224,4,224,1002,223,8,223,101,1,224,224,1,223,224,223,1102,31,84,225,1102,29,77,225,1,176,188,224,101,-42,224,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,2,196,183,224,1001,224,-990,224,4,224,1002,223,8,223,101,7,224,224,1,224,223,223,102,14,40,224,101,-1078,224,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1001,180,64,224,101,-128,224,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,1102,24,17,224,1001,224,-408,224,4,224,1002,223,8,223,101,2,224,224,1,223,224,223,1101,9,66,224,1001,224,-75,224,4,224,1002,223,8,223,1001,224,6,224,1,223,224,223,1102,18,33,225,1101,57,64,225,1102,45,11,225,1101,45,9,225,1101,11,34,225,1102,59,22,225,101,89,191,224,1001,224,-100,224,4,224,1002,223,8,223,1001,224,1,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,226,677,224,1002,223,2,223,1006,224,329,1001,223,1,223,108,226,226,224,1002,223,2,223,1006,224,344,1001,223,1,223,7,677,226,224,102,2,223,223,1005,224,359,101,1,223,223,7,226,677,224,102,2,223,223,1006,224,374,101,1,223,223,1008,677,226,224,1002,223,2,223,1006,224,389,101,1,223,223,8,677,677,224,1002,223,2,223,1005,224,404,101,1,223,223,8,677,226,224,102,2,223,223,1005,224,419,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,434,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,449,1001,223,1,223,107,677,226,224,1002,223,2,223,1005,224,464,1001,223,1,223,1008,677,677,224,1002,223,2,223,1006,224,479,1001,223,1,223,1108,677,226,224,1002,223,2,223,1006,224,494,1001,223,1,223,1108,677,677,224,1002,223,2,223,1006,224,509,1001,223,1,223,107,677,677,224,1002,223,2,223,1005,224,524,101,1,223,223,1007,677,226,224,102,2,223,223,1005,224,539,1001,223,1,223,1107,226,226,224,1002,223,2,223,1006,224,554,1001,223,1,223,1008,226,226,224,1002,223,2,223,1006,224,569,101,1,223,223,1108,226,677,224,1002,223,2,223,1006,224,584,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,599,1001,223,1,223,1007,677,677,224,102,2,223,223,1006,224,614,101,1,223,223,107,226,226,224,102,2,223,223,1006,224,629,101,1,223,223,1007,226,226,224,102,2,223,223,1005,224,644,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,659,1001,223,1,223,7,677,677,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226]

main =
    undefined



isRight (Right _) = True
isRight _ = False
