{-analisador lexico-}

module Lexer (scan) where

import Token
import Data.Char (isAlpha, isAlphaNum, isDigit, toLower, isSpace)

scan :: String -> [Token]
-- your implementation here
