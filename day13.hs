import qualified Data.Map as M
import Data.List

import Intcode

runGame game =
    let output = runProgram game
        stepGame (x,y,c) = M.insert (x,y) c
        triples (x:y:c:rest) = (x,y,c) : triples rest
    in
    foldl' stepGame M.empty triples
