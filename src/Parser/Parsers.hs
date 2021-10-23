module Parser.Parsers where


import           Data.Char
import           Parser.Basic
import           Parser.Conbinators

-- * parse a single character
pChar :: PSource Char
pChar = bParser

-- * parse a specific character
pLiteral :: Char -> PSource Char
pLiteral c = pChar <=> (== c)

-- * parse a single digit
pDigit :: PSource Char
pDigit = pChar <=> isDigit

-- * parse series of digits
pDigits :: PSource String
pDigits = cIterSafe pDigit

-- * parse a single letter
pLetter :: PSource Char
pLetter = pChar <=> isAlpha

-- * parse series of letters
pLetters :: PSource String
pLetters = cIterSafe pLetter

-- * parse a single digit or letter
pDigitLetter :: PSource Char
pDigitLetter = pDigit <|> pLetter

-- * parse a single punctuation
pPunctuation :: PSource Char
pPunctuation = pChar <=> (`elem` "+-=*/%|&!><")

-- * parse series of punctuation
pPunctuations :: PSource String
pPunctuations = cIterSafe pPunctuation

-- * parse a single space character
pSpace :: PSource Char
pSpace = pChar <=> isSpace

-- * modify: remove space behind a string
mRmSpace :: PSource a -> PSource a
mRmSpace p = p <+-> cIter pSpace
