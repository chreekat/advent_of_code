import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import System.IO.Unsafe

main = print (part1, part2)

input = unsafePerformIO (readFile "day6-input.txt")

head' :: Foldable t => t a -> Maybe a
head' = getFirst . foldMap (First . Just)

groupAnswers = splitOn [""] . lines

reducedAnswers1 :: Ord a => [[a]] -> [[a]]
reducedAnswers1 = group . sort . concat

reducedAnswers2 :: Ord a => [[a]] -> [[a]]
reducedAnswers2 individualAnswers =
    let tot = length individualAnswers
    in filter ((== tot) . length) (reducedAnswers1 individualAnswers)

summarized reducer =
    sum (map (length . simplifyAnswers . reducer) (groupAnswers input))

simplifyAnswers :: Eq a => [[a]] -> [a]
simplifyAnswers = catMaybes . map head'

part1 = summarized reducedAnswers1

part2 = summarized reducedAnswers2
