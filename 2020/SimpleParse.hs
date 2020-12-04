module SimpleParse (module X, module SimpleParse, L.decimal, L.hexadecimal) where

import Data.Void
import Text.Megaparsec as X
import Text.Megaparsec.Char as X
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

p_char :: Char -> Parser Char
p_char = char

p_int :: Parser Int
p_int = decimal

p_string :: String -> Parser String
p_string = string

pInt = parseMaybe p_int

