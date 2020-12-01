import qualified Data.Map as M
import Data.List

import Text.Pretty.Simple

import Intcode

runGame game =
    let output = runProgram game []
        stepGame st (x,y,c) = M.insert (x,y) c st
        triples [] = []
        triples (x:y:c:rest) = (x,y,c) : triples rest
    in
    foldl' stepGame M.empty (triples output)

main = part1

part1 = do
    prog <- readProgramFile "day13-input"
    let sta = runGame prog
    pPrint (M.size (M.filter (== 2) sta))

