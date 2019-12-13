{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-unused-top-binds #-}

import Data.Function
import Data.List
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
    , computerInput :: [Int]
    , computerOutput :: [Int]
    } deriving (Eq, Show)

evalStep :: Computer -> Computer
evalStep (Computer _ mem@(Memory ram pc) ins outs)
    = case readOpCode =<< M.lookup pc ram of
        Just (Op Halt _) -> Computer Halted mem ins outs
        Nothing -> Computer Error mem ins outs
        Just (Op Add ps) -> Computer Running (add mem ps) ins outs
        Just (Op Mul ps) -> Computer Running (mul mem ps) ins outs
        Just (Op Input ps) -> 
            case ins of
                [] -> error "no input for Input"
                (i:ii) -> Computer Running (inp mem ps i) ii outs
        Just (Op Output ps) ->
            let (newmem, o) = out mem ps
            in Computer Running newmem ins (o:outs)
        Just (Op JumpT ps) -> Computer Running (jmpt mem ps) ins outs
        Just (Op JumpF ps) -> Computer Running (jmpf mem ps) ins outs
        Just (Op StoreLT ps) -> Computer Running (stolt mem ps) ins outs
        Just (Op StoreEQ ps) -> Computer Running (stoeq mem ps) ins outs

add, mul :: Memory -> [ParamMode] -> Memory
add = binOp (+)
mul = binOp (*)

binOp :: (Int -> Int -> Int) -> Memory -> [ParamMode] -> Memory
binOp op (Memory ram pc) ps =
    let [a1,a2] = zipWith (deref . Memory ram) [pc+1, pc+2] ps
        cp = ram M.! (pc + 3)
    in Memory (M.insert cp (a1 `op` a2) ram) (pc + 4)

inp :: Memory -> [ParamMode] -> Int -> Memory
inp (Memory ram pc) _ input =
    let cp = ram M.! (pc + 1)
    in Memory (M.insert cp input ram) (pc+2)

out :: Memory -> [ParamMode] -> (Memory, Int)
out (Memory ram pc) [p] = (Memory ram (pc+2), deref (Memory ram (pc + 1)) p)
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
isRunning (Computer Running _ _ _) = True
isRunning _ = False

runProgram :: Program -> [Int] -> ([Int],[Int])
runProgram program input =
    let mem0 = Memory (M.fromList (zip [0..] program)) 0
        step nxt c
            | isRunning c = nxt (evalStep c)
            | otherwise = pure c
    in do
        c@(Computer resStat resRam _ outs) <- fix step (Computer Running mem0 input [])
        case resStat of
            -- elems gives ascending order
            Halted -> (M.elems (memRam resRam), outs)
            _ -> error (show c)

type PhaseSetting = Int
type Program = [Int]

runAmpCircuit :: Program -> [PhaseSetting] -> Int
runAmpCircuit prog = 
    -- folding over the phasesetting list.
    -- the accumulator is the output of the previous amp.
    -- acc0 is 0.
    foldl' (\input phase -> head (snd (runProgram prog [phase, input]))) 0
    
main = do
    prog <- readProgramFile "day7-input"
    print $ maximum $ map (runAmpCircuit prog) $ permutations [0..4]

readProgramFile f = read @[Int] . ("["++) . (++"]") . head . lines <$> readFile f

isRight (Right _) = True
isRight _ = False
