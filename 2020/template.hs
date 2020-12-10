{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import Data.Bifunctor
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import System.IO.Unsafe
import qualified Data.Vector as V

import SimpleParse

test = unsafePerformIO (readFile "day10-test.txt")
input = unsafePerformIO (readFile "day10-input.txt")

main = do
    print part1test
    print part1actual
    print part2test
    print part2actual

part1test = "hi"
part1actual = "hi"
part2test = "hi"
part2actual = "hi"
