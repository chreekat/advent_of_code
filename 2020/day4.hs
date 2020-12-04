import Data.Bool
import Data.List
import Data.List.Split
import Data.Maybe
import SimpleParse
import qualified Data.Map as Map


main = do
    input <- readFile "day4-input.txt"
    print (part1 input, part2 input)
{-

    byr (Birth Year)
    iyr (Issue Year)
    eyr (Expiration Year)
    hgt (Height)
    hcl (Hair Color)
    ecl (Eye Color)
    pid (Passport ID)
    cid (Country ID)

-}

pCode s 
    | s `elem`
        [ "byr"
        , "iyr"
        , "eyr"
        , "hgt"
        , "hcl"
        , "ecl"
        , "pid"
        , "cid"
        ]
    = Just s
    | otherwise = Nothing

pField s =
    let (sc, sv) = break (== ':')  s
    in (,) <$> pCode sc  <*> safeTail sv


safeTail [] = Nothing
safeTail (_:xs) = Just xs


pPassport = fmap Map.fromList . traverse pField . words

pInput = traverse (pPassport . unlines) . splitOn [""] . lines


validatePassport1 = (== 8) . Map.size . Map.insert "cid" ""

part1 = lol validatePassport1

lol f = sum . map (bool 0 1 . f) . fromMaybe [] . pInput

validatePassport2 x = all id $ map (fromMaybe False) (map ($ x) [validByr, validIyr, validEyr, validHgt, validHcl, validEcl, validPid])

validByr x = rangeInc 1920 2002 <$> (pInt =<< x Map.!? "byr")
validIyr x = rangeInc 2010 2020 <$> (pInt =<< x Map.!? "iyr")
validEyr x = rangeInc 2020 2030 <$> (pInt =<< x Map.!? "eyr")
validHgt x = case pHgt =<< x Map.!? "hgt" of
    Just (n, "cm") -> Just $ rangeInc 150 193 n
    Just (n, "in") -> Just $ rangeInc 59 76 n
    _ -> Nothing
validHcl x = True <$ (pHcl =<< x Map.!? "hcl")
validEcl x = True <$ (pEcl =<< x Map.!? "ecl")
validPid x = True <$ (pPid =<< x Map.!? "pid")

pPid = parseMaybe (lookAhead (count 9 digitChar >> eof) >> p_int)

pHcl = parseMaybe $ do
    (:) <$> p_char '#' <*> count 6 hexDigitChar

pHgt = parseMaybe $ do
    ht <- p_int
    dim <- many letterChar
    pure (ht, dim)

pEcl = parseMaybe $ foldr1 (<|>) (map p_string ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

rangeInc hi lo x = hi <= x && x <= lo

part2 = lol validatePassport2
