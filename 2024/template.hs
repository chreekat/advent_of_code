{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant map" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
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
import Control.Monad.Writer.CPS as Writer
import Data.Foldable qualified as Fold
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import HMap qualified
import Map qualified
import Seq qualified
import Tropes hiding (traceShow, traceShowId)
import Tropes qualified
import TwoD qualified
import Data.List.Split qualified as Split

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "XXX-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "XXX.txt")

ans1 :: _
ans1 = ex1

ans2 :: _
ans2 = undefined

main = print ans1
