{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

import Data.Foldable
import Data.Function
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import GHC.Generics
import Test.QuickCheck
import qualified Data.Map as M

data Cmd = Add | Mul | Halt

opcodes :: Map Int Cmd
opcodes = M.fromList [(1, Add), (2, Mul), (99, Halt)]

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
evalStep mem@(Memory ram pc) = case (flip M.lookup opcodes =<< M.lookup pc ram) of
    Just Add -> Right $ Computer Running (add mem)
    Just Mul -> Right $ Computer Running (mul mem)
    Just Halt -> Left $ Computer Halted mem
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


evalProgram :: [Int] -> [Int]
evalProgram input =
    let mem0 = Memory (M.fromList (zip [0..] input)) 0
        c0 = Computer Running mem0
        step nxt (Left c) = c
        step nxt (Right (Computer _ mem)) = nxt (evalStep mem)
    in M.elems $ memRam $ computerMemory $ fix step (Right (Computer Running mem0))


honk = [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,2,10,23,27,1,27,6,31,1,13,31,35,1,13,35,39,1,39,10,43,2,43,13,47,1,47,9,51,2,51,13,55,1,5,55,59,2,59,9,63,1,13,63,67,2,13,67,71,1,71,5,75,2,75,13,79,1,79,6,83,1,83,5,87,2,87,6,91,1,5,91,95,1,95,13,99,2,99,6,103,1,5,103,107,1,107,9,111,2,6,111,115,1,5,115,119,1,119,2,123,1,6,123,0,99,2,14,0,0]

runInput noun verb =
    let (x:_:_:inp) = honk
    in head $ evalProgram (x:noun:verb:inp)

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


idOpsChangeNothing (Sensible mem) = case evalStep mem of
    Right (Computer Running mem') -> label "Running" (mem == mem')
    Left (Computer Halted mem'@(Memory ram pc)) -> label "Halted" $ mem == mem' && ram M.! 0 == 99
    Left (Computer Error mem') -> label "Error" $ mem == mem'

