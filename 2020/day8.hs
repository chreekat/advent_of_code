{-# LANGUAGE FlexibleContexts #-}

import Data.Bifunctor
import Data.Monoid
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import System.IO.Unsafe
import qualified Data.Vector as V

import SimpleParse

main = print (part1 input, part2 input)


acc v = bimap (+ 1) (+ v)
nop _ = bimap (+ 1) id
jmp v = bimap (+ v) id

exec cmd = do
    case cmd of "acc" -> acc
                "nop" -> nop
                "jmp" -> jmp


step :: (Monad m, MonadState (V.Vector Bool, (Int, Int)) m, MonadReader (V.Vector (String, Int)) m, MonadError Int m) => m ()
step = do
    (visits, (deref, acc)) <- get
    if visits V.! deref
        then throwError acc
        else () <$ do
            (cmd, val) <- asks (V.! deref)
            modify (second (exec cmd val) . first (V.// [(deref, True)]))


test = unsafePerformIO (readFile "day8-test.txt")
input = unsafePerformIO (readFile "day8-input.txt")

mkPair [a,b] = (a,b)

part1 :: String -> Int
part1 = left . runProgram (forever step) . mkInstrs


mkInstrs = V.fromList . map (second (parse' signedInt) . mkPair . words) . lines

runProgram stepper instrs =
    runExcept (runStateT (runReaderT stepper instrs) (V.map (const False) instrs, (0, 0)))


allPrograms :: [(String, b)] -> [[(String, b)]]
allPrograms [] = [[]]
allPrograms (instr@(cmd, val) : rest) =
    let allSubProgs = map (instr :) (allPrograms rest)
    in case cmd of 
        "acc" -> allSubProgs
        "nop" -> (("jmp", val) : rest) : allSubProgs
        "jmp" -> (("nop", val) : rest) : allSubProgs

step2 :: (Monad m, MonadState (V.Vector Bool, (Int, Int)) m, MonadReader (V.Vector (String, Int)) m, MonadError (Maybe Int) m) => m ()
step2 = do
    (visits, (deref, acc)) <- get
    if deref == V.length visits
        then throwError (Just acc)
        else
            if visits V.! deref
                then throwError Nothing
                else () <$ do
                    (cmd, val) <- asks (V.! deref)
                    modify (second (exec cmd val) . first (V.// [(deref, True)]))


left (Left e) = e
left _ = error "Sinister!"

part2 :: String -> Maybe Int
part2 = getFirst . foldMap (First . left . runProgram (forever step2)) . map V.fromList . allPrograms . V.toList . mkInstrs
