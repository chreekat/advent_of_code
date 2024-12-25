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

import Prelude hiding (lookup)
import Array qualified
import System.IO qualified as Sys
import Control.Monad qualified as Monad
import Control.Monad.State qualified as State
import Control.Monad.Writer.CPS as Writer
import Data.Foldable qualified as Fold
import Data.List qualified as List
import Data.List.Split qualified as Split
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Vector qualified as Vector
import HMap qualified
import Map qualified
import Seq qualified
import Set qualified
import Tropes hiding (traceShow, traceShowId, range,(!))
import Tropes qualified
import TwoD qualified

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
