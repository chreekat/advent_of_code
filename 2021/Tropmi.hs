module Tropmi (foldl', unsafePerformIO, module Tropmi) where
import Data.List
import System.IO.Unsafe

sum3 (a,b,c) = sum [a,b,c]
