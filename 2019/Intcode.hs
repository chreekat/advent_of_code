{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-unused-top-binds #-}

module Intcode (readProgramFile, Program, runProgram) where

import Data.Function
-- import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

-- import Debug.Pretty.Simple

data Cmd
    = Add
    | Mul
    | Input
    | Output
    | JumpT
    | JumpF
    | StoreLT
    | StoreEQ
    | AdjRelRef
    | Halt
    deriving Show

-- | Would be hidden in a real program
data OpCode a = Op Cmd a

deriving instance Show a => Show (OpCode a)

data ParamMode = Pos | Imm | Rel
    deriving (Eq, Show)

mode :: Int -> ParamMode
mode 0 = Pos
mode 1 = Imm
mode 2 = Rel
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
    ,(9, Op AdjRelRef 1)
    ,(99, Op Halt 0)
    ]

readOpCode :: Int -> Maybe (OpCode [ParamMode])
readOpCode code = do
    Op cmd paramCt <- M.lookup (code `mod` 100) opcodes
    let modes = map mode . map (\i -> code `div` 10^i `mod` 10) $ take paramCt ([2..] :: [Int])
    pure (Op cmd modes)

-- | Relative offset for location of the ptr.
readP :: Memory -> [ParamMode] -> Int -> Int
readP (Memory ram pc o) pms idx =
    let imm = ram M.! (pc + idx + 1)
    in case safeIndex pms idx of
        Nothing -> error "readP: bad param index"
        Just Imm -> imm
        Just Pos -> lookupWithDefault 0 imm ram
        Just Rel -> lookupWithDefault 0 (o + imm) ram
  where
    lookupWithDefault d k =
        if k < 0 then error "HCF in readP" else fromMaybe d . M.lookup k

safeIndex a i = if length a <= i then Nothing else Just (a !! i)

writeP :: Memory -> [ParamMode] -> Int -> Int -> Ram
writeP (Memory ram pc o) pms idx val =
    let imm = ram M.! (pc + idx + 1)
        cp = case safeIndex pms idx of
            Nothing -> error "writeP: bad param index"
            Just Imm -> error "HCF in writeP"
            Just Pos -> imm
            Just Rel -> o + imm
    in M.insert cp val ram

type Ram = Map Int Int
type PC = Int

data Memory = Memory
    { memRam :: Ram
    , memPC :: PC
    , memRelOffset :: Int
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
evalStep (Computer _ mem@(Memory ram pc _o) ins outs)
    = case readOpCode =<< M.lookup pc ram of
        Nothing -> Computer Error mem ins outs
        Just (Op op params) -> case op of
            Halt -> Computer Halted mem ins outs
            Add -> Computer Running (add mem params) ins outs
            Mul -> Computer Running (mul mem params) ins outs
            Input ->
                case ins of
                    [] -> error "no input for Input"
                    (i:ii) -> Computer Running (inp mem params i) ii outs
            Output ->
                let (newmem, o) = out mem params
                in Computer Running newmem ins (Just o)
            JumpT -> Computer Running (jmpt mem params) ins outs
            JumpF -> Computer Running (jmpf mem params) ins outs
            StoreLT -> Computer Running (stolt mem params) ins outs
            StoreEQ -> Computer Running (stoeq mem params) ins outs
            AdjRelRef -> Computer Running (adjrel mem params) ins outs

adjrel :: Memory -> [ParamMode] -> Memory
adjrel mem@(Memory ram pc o) p = Memory ram (pc + 2) (o + readP mem p 0)

add, mul :: Memory -> [ParamMode] -> Memory
add = binOp (+)
mul = binOp (*)

binOp :: (Int -> Int -> Int) -> Memory -> [ParamMode] -> Memory
binOp op mem@(Memory _ pc o) ps =
    let [a1,a2] = map (readP mem ps) [0,1]
    in Memory (writeP mem ps 2 (a1 `op` a2)) (pc + 4) o

inp :: Memory -> [ParamMode] -> Int -> Memory
inp mem@(Memory _ pc o) p input = Memory (writeP mem p 0 input) (pc+2) o

out :: Memory -> [ParamMode] -> (Memory, Int)
out mem@(Memory ram pc o) p = (Memory ram (pc+2) o, readP mem p 0) -- AAAH

jmpt, jmpf :: Memory -> [ParamMode] -> Memory
jmpt = jmpCond (/= 0)
jmpf = jmpCond (== 0)

jmpCond :: (Int -> Bool) -> Memory -> [ParamMode] -> Memory
jmpCond cond mem@(Memory ram pc o) ps =
    case map (readP mem ps) [0,1] of
        [t,a] -> Memory ram (if cond t then a else pc + 3) o
        _ -> error "HCF in jmpCond"

stoeq, stolt :: Memory -> [ParamMode] -> Memory
stoeq = stoCmp (==)
stolt = stoCmp (<)

stoCmp :: (Int -> Int -> Bool) -> Memory -> [ParamMode] -> Memory
stoCmp cond mem@(Memory _ pc o) pms =
    case map (readP mem pms) [0,1] of
        [a1,a2] ->
            Memory
                (writeP mem pms 2 (if cond a1 a2 then 1 else 0))
                (pc + 4)
                o
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
    let mem0 = Memory (M.fromList (zip [0..] program)) 0 0
        step nxt c
            | isRunning c = computerOutput c : nxt (evalStep (c { computerOutput = Nothing }))
            | otherwise = [computerOutput c]
    in catMaybes $ fix step (Computer Running mem0 input Nothing)

type Program = [Int]

readProgramFile f = read @[Int] . ("["++) . (++"]") . head . lines <$> readFile f
