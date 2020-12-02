
import Control.Monad (void)
import Data.Maybe
import Data.Vector ((!?), fromList)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L

main = putStrLn "Hello world"

type Parser = Parsec Void String

type RuleDat = (Int, Int, Char)
type PW = String

type Rule = RuleDat -> PW -> Bool


pass :: Rule
pass (lo, hi, c) pw =
    let num = length (filter (== c) pw)
    in lo <= num && num <= hi


lineParser :: Parser (RuleDat, PW)
lineParser = do
    lo <- decimal
    void (char '-')
    hi <- decimal
    void (char ' ')
    c <- letterChar
    void (string ": ")
    pass <- some charLiteral
    pure ((lo, hi, c), pass)


part1 = do
    input <- readFile "day2-input.txt"
    let ls = lines input
    let stuff = mapMaybe (parseMaybe lineParser) ls
    let tests = map (uncurry pass) stuff
    let result = length (filter id tests)
    pure result

pass2 :: Rule
pass2 (lo, hi, c) pw =
    let xor a b = (a || b) && (not a || not b)
        pw' = fromList pw
        a = Just c == (pw' !? (lo - 1))
        b = Just c == (pw' !? (hi - 1))
    in a `xor` b

part2 = do
    input <- readFile "day2-input.txt"
    let ls = lines input
    let stuff = mapMaybe (parseMaybe lineParser) ls
    let tests = map (uncurry pass2) stuff
    let result = length (filter id tests)
    pure result

