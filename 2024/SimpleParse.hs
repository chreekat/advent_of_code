{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module SimpleParse (module X, module SimpleParse) where

import Data.Maybe
import Data.Void
import Text.Megaparsec as X
import Text.Megaparsec.Char as X
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

parse' p = fromJust . parseMaybe p

lexeme = L.lexeme spaceConsumer
decimal = lexeme L.decimal

signedInt :: Parser Int
signedInt = L.signed spaceConsumer decimal

symbol = L.symbol spaceConsumer
