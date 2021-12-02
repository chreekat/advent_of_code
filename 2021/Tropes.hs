module Tropes (foldl', unsafePerformIO, module Tropes) where
import Data.List
import System.IO.Unsafe

sum3 (a,b,c) = sum [a,b,c]

prod2 (a,b) = product [a,b]

_3to2 (a,b,c) = (a,b)
