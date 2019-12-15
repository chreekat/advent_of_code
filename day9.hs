{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-unused-top-binds #-}

import Data.Function
import Data.List
import Data.Maybe
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
    , computerOutput :: Maybe Int
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
            in Computer Running newmem ins (Just o)
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

runProgram :: Program -> [Int] -> [Int]
runProgram program input =
    let mem0 = Memory (M.fromList (zip [0..] program)) 0
        step nxt c
            | isRunning c = computerOutput c : nxt (evalStep (c { computerOutput = Nothing }))
            | otherwise = [computerOutput c]
    in catMaybes $ fix step (Computer Running mem0 input Nothing)

type PhaseSetting = Int
type Program = [Int]

runAmpCircuit :: Program -> [PhaseSetting] -> Int
runAmpCircuit prog =
    -- folding over the phasesetting list.
    -- the accumulator is the output of the previous amp.
    -- acc0 is 0.
    foldl' (\input phase -> head (runProgram prog [phase, input])) 0

main0 = do
    prog <- readProgramFile "day7-input"
    print $ maximum $ map (runAmpCircuit prog) $ permutations [0..4]

main = do
    prog <- readProgramFile "day7-input"
    print $ maximum $ map (runFeedbackAmpCircuit prog) $ permutations [5..9]

readProgramFile f = read @[Int] . ("["++) . (++"]") . head . lines <$> readFile f

runFeedbackAmpCircuit :: Program -> [PhaseSetting] -> Int
runFeedbackAmpCircuit prog phases =
    let input0 = (phases !! 0) : 0 : outputs4
        input1 = (phases !! 1) : outputs0
        input2 = (phases !! 2) : outputs1
        input3 = (phases !! 3) : outputs2
        input4 = (phases !! 4) : outputs3
        outputs0 = runProgram prog input0
        outputs1 = runProgram prog input1
        outputs2 = runProgram prog input2
        outputs3 = runProgram prog input3
        outputs4 = runProgram prog input4
    in last outputs4

dummy2, dummy :: Program
dummy = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

dummy2 = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54, -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4, 53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
