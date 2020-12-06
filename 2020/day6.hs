import Data.List
import Data.List.Split
import qualified Data.List.NonEmpty as NE

main = do
    input <- readFile "day6-input.txt"
    let reducedAnswers1 = NE.group . sort . concat
        reducedAnswers2 individualAnswers =
            let tot = length individualAnswers
            in filter ((== tot) . length) (reducedAnswers1 individualAnswers)
        summarized reducer =
            sum . map (length . map NE.head . reducer) . splitOn [""] . lines
        part1 = summarized reducedAnswers1 input
        part2 = summarized reducedAnswers2 input
    print (part1, part2)
