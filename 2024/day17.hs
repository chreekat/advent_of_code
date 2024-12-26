{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant map" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Array qualified
import Control.Applicative as Appl
import Control.Monad.Identity
import Control.Monad qualified as Monad
import Control.Monad.State qualified as State
import Control.Monad.Trans qualified as Monad
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Maybe qualified as Maybe
import Control.Monad.Writer.CPS as Writer
import Control.Monad.Trans.Writer.CPS as Writer (runWriterT)
import Data.Bits qualified as Bits
import Data.Foldable qualified as Fold
import Data.List qualified as List
import Data.List.Split qualified as Split
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Vector qualified as Vector
import HMap qualified
import Map qualified
import Prelude hiding (lookup)
import Seq qualified
import Set qualified
import SimpleParse qualified as Parse
import System.IO qualified as Sys
import Tropes hiding (traceShow, traceShowId, range,(!))
import Tropes qualified
import TwoD qualified

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day17-ex2.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day17.txt")

parse = do
    _ <- Parse.symbol "Register A:"
    a <- Parse.decimal
    _ <- Parse.symbol "Register B:"
    b <- Parse.decimal
    _ <- Parse.symbol "Register C:"
    c <- Parse.decimal
    _ <- Parse.symbol "Program:"
    p <- Parse.sepBy Parse.decimal (Parse.symbol ",")
    pure (a,b,c,Seq.fromList p)



proc :: (Int,Int,Int,Seq Int) -> Int -> MaybeT (Writer [Int]) (Int,Int,Int,Seq Int,Int)
proc (a,b,c,mem) i = do
    -- lift a maybe into a WriterT Maybe
    opcode <- Maybe.hoistMaybe $ mem Seq.!? i
    operand <- Maybe.hoistMaybe $ mem Seq.!? (i + 1)
    go opcode operand
  where
    go 0 opr = pure (a `div` (2 ^ combo (a,b,c) opr), b, c, mem, i+2)
    go 1 opr = pure (a, b `Bits.xor` opr,c, mem, i+2)
    go 2 opr = pure (a, combo (a,b,c) opr `mod` 8, c, mem, i+2)
    go 3 opr = pure (a,b,c,mem, if a == 0 then i+2 else opr)
    go 4 _   = pure (a, b `Bits.xor` c, c, mem, i+2)
    go 5 opr = lift (Writer.tell [combo (a,b,c) opr `mod` 8]) >> pure (a,b,c,mem,i+2)
    go 6 opr = pure (a, a `div` 2 ^ combo (a,b,c) opr, c, mem, i+2)
    go 7 opr = pure (a, b, a `div` 2 ^ combo (a,b,c) opr, mem, i+2)
    go opc opr = error $ "Bad opc or opr: " ++ show (opc,opr)

-- run :: (Int,Int,Int,Seq Int) -> Int -> Writer [Int] (Int,Int,Int,Seq Int,Int)
run x i = do
    (a,b,c,mem,i') <- proc x i
    run (traceShowId (a,b,c,mem)) (traceShowId i')

traceShowId = id -- Tropes.traceShowId

makeOutput :: [Int] -> String
makeOutput = List.intercalate "," . map show

ans1 :: _
ans1 = case Parse.parse parse mempty dat
    of
        Left e -> Parse.errorBundlePretty e
        Right x -> makeOutput $ Writer.execWriter $ Maybe.runMaybeT $
            run x 0
            -- run (2024,29,0, Seq.fromList [1,7]) 0

combo (a,b,c) i
    | 0 <= i && i <= 3 = i
    | i == 4 = a
    | i == 5 = b
    | i == 6 = c
    | otherwise = error $ "bad combo operand: " ++ show i


proc2 :: (Int,Int,Int,Seq Int,[Int]) -> Int -> MaybeT Identity (Int,Int,Int,Seq Int,Int,[Int])
proc2 (a,b,c,mem,target) i = do
    -- lift a maybe into a WriterT Maybe
    opcode <- Maybe.hoistMaybe $ mem Seq.!? i
    operand <- Maybe.hoistMaybe $ mem Seq.!? (i + 1)
    go opcode operand
  where
    go 0 opr = pure (a `div` (2 ^ combo (a,b,c) opr), b, c, mem, i+2, target)
    go 1 opr = pure (a, b `Bits.xor` opr,c, mem, i+2, target)
    go 2 opr = pure (a, combo (a,b,c) opr `mod` 8, c, mem, i+2, target)
    go 3 opr = pure (a,b,c,mem, if a == 0 then i+2 else opr, target)
    go 4 _   = pure (a, b `Bits.xor` c, c, mem, i+2, target)
    go 5 opr = 
        let out = combo (a,b,c) opr `mod` 8
        in case target of
            [] -> fail "oh"
            (t:ts) | t == out -> pure (a,b,c,mem,i+2, ts)
                | otherwise -> fail "oh"

    go 6 opr = pure (a, a `div` 2 ^ combo (a,b,c) opr, c, mem, i+2, target)
    go 7 opr = pure (a, b, a `div` 2 ^ combo (a,b,c) opr, mem, i+2, target)
    go opc opr = error $ "Bad opc or opr: " ++ show (opc,opr)

-- run :: (Int,Int,Int,Seq Int) -> Int -> Writer [Int] (Int,Int,Int,Seq Int,Int)
run2 x i = do
    (a,b,c,mem,i', ts) <- proc2 x i
    case ts of [] -> pure x
               _ -> run2 (traceShowId (a,b,c,mem,ts)) (traceShowId i')

run3 q@(_,b,c,mem) a =
    a <$ run2 (a,b,c,mem, toList mem) 0 <|> run3 q (a+1)

ans2 :: _
ans2 = case Parse.parse parse mempty dat
    of
        Left e -> Parse.errorBundlePretty e
        Right x@(a,b,c,mem) -> show $ runIdentity $ Maybe.runMaybeT $
            run3 (a,b,c,mem) 0
            -- run (2024,29,0, Seq.fromList [1,7]) 0

main = putStrLn ans2
