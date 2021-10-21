module Parser.Parsers where


import           Data.Char
import           Parser.Basic
import           Parser.Conbinators


pChar :: Parser Char
pChar ""     = Nothing
pChar (c:cs) = Just (c, cs)

pDigit :: Parser Char
pDigit = pChar <=> isDigit

pLetter :: Parser Char
pLetter = pChar <=> isAlpha

pSpace :: Parser Char
pSpace = pChar <=> isSpace

