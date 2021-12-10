{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

import Tropes
import SimpleParse

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

data ParseRes = Incomplete | Corrupted Char | Ok
    deriving (Eq, Show)

parseLine l = case parse (some p_chunk) "" l of
    Right _ -> Ok
    Left (ParseErrorBundle (TrivialError _ err _ :|[]) _)
        | Just EndOfInput <- err -> Incomplete
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

ans2 :: _
ans2 = undefined

-- ghcid needs this?
main = undefined
