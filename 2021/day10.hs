{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}

import SimpleParse
import Tropes

ex1, dat :: String
ex1 = unsafePerformIO (readFile "day10-ex1.txt")
dat = unsafePerformIO (readFile "day10.txt")

surroundChar a b p = p_string a >> p >> p_string b

p_chunk =
    surroundChar "[" "]" (many p_chunk)
        <|> surroundChar "(" ")" (many p_chunk)
        <|> surroundChar "{" "}" (many p_chunk)
        <|> surroundChar "<" ">" (many p_chunk)

p_line = some p_chunk

p_subsystem = fmap (parse p_line "") . lines

data ParseRes a = Incomplete a | Corrupted Char | Ok
    deriving (Eq, Show)

parseLine l = case parse (some p_chunk) "" l of
    Right _ -> Ok
    Left (ParseErrorBundle (TrivialError _ err expected :| []) bundle)
        | Just EndOfInput <- err -> Incomplete (complete (some p_chunk) expected bundle)
        | Just (Tokens (c :| [])) <- err -> Corrupted c

parseSubsystem = fmap parseLine . lines

failScore (Corrupted c)
    | ')' <- c = 3
    | ']' <- c = 57
    | '}' <- c = 1197
    | '>' <- c = 25137
failScore _ = 0

ans1 :: _
ans1 = sum $ map failScore (parseSubsystem dat)

stepComplete p expected bundle =
    let missingChar = head . filter (`elem` ")]}>") $ extractExpected expected
        l' = pstateInput bundle
        l = l' <> [missingChar]
        st = State l 0 (bundle{pstateInput = l' <> [missingChar]}) []
        extractExpected = map (head . toList . (\(Tokens s) -> s)) . toList
     in (missingChar, runParser' p st)

complete p expected bundle =
    let (c, (_, res)) = stepComplete p expected bundle
     in case res of
            Right _ -> [c]
            Left (ParseErrorBundle (TrivialError _ err expected' :| []) bundle') ->
                c : complete p expected' bundle'

completeScore (Incomplete str) = Just $ foldl' f 0 str
  where
    f s c = s * 5 + fromJust (lookup c (zip ")]}>" [1 ..]))
completeScore _ = Nothing

sysCompleteScore s =
    let scores = mapMaybe completeScore (parseSubsystem s)
        l = length scores
     in head $ drop (l `div` 2) $ sort scores

ans2 :: _
ans2 = sysCompleteScore dat

-- ghcid needs this?
main = undefined
