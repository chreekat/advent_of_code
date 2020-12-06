import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import System.IO.Unsafe

main = print (part1, part2)

input = unsafePerformIO (readFile "day6-input.txt")

groupAnswers = splitOn [""] . lines

combinedAnswers :: Ord a => [[a]] -> [[a]]
combinedAnswers = group . sort . concat

simplifyAnswers :: Eq a => [[a]] -> [a]
simplifyAnswers = catMaybes . map head'

head' :: Foldable t => t a -> Maybe a
head' = getFirst . foldMap (First . Just)

part1 = lol combinedAnswers

lol f = sum (map (length . simplifyAnswers . f) (groupAnswers input))

combinedAnswers' :: Ord a => [[a]] -> [[a]]
combinedAnswers' individualAnswers =
    let tot = length individualAnswers
    in filter ((== tot) . length) (combinedAnswers individualAnswers)

part2 = lol combinedAnswers'
