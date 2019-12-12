import Data.List

input = [146810..612564]

canPassword n = let chars = show n in monotonic chars && hasDup chars

monotonic chars = chars == sort chars

hasDup = any (== 2) . map length . group 
main = print $ length $ filter canPassword input
