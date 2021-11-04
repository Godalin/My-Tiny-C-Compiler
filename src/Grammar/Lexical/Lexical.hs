module Grammar.Lexical.Lexical
    ( genTokenStream
    ) where

import           Grammar.Lexical.Basic
import           Parser.Basic
import           Parser.Conbinators
import           Parser.Parsers


genTokenStream :: String -> [Token]
genTokenStream source = case pTokenStream source of
    Just (ts, "") -> ts
    _             -> error "Wrong Source Code"

pTokenStream :: PSource [Token]
pTokenStream = cIterSafe pToken

pToken :: TParser
pToken = pKeyWord
        <|> pIdentifier
        <|> pOperator
        <|> pIntConstant
        <|> pStringConstant
        <|> pSinglePunc

pIdentifier :: TParser
pIdentifier = mRmSpace (pLetter <+> cIter pDigitLetter
                >>> uncurry (:))
                >>> TIdentifier

pIntConstant :: TParser
pIntConstant = mRmSpace pDigits
                >>> TIntConstant

pStringConstant :: TParser
pStringConstant = mRmSpace (cEncloseEx
                (pLiteral '"')
                (pLiteral '"')
                (cIter (
                    pLiteral '\\' <-+> (pChar >>> escape)
                    <|> pChar <=> (/= '"'))))
                >>> TStringConstant

escape :: Char -> Char
escape 't'  = '\t'
escape 'n'  = '\n'
escape 'r'  = '\r'
escape 'b'  = '\b'
escape '"'  = '"'
escape '\'' = '\''
escape '\\' = '\\'
escape _    = '?'

pKeyWord :: TParser
pKeyWord = mRmSpace pLetters +> transform where
    transform "void"     = cJust TVoid
    transform "int"      = cJust TInt
    transform "while"    = cJust TWhile
    transform "if"       = cJust TIf
    transform "else"     = cJust TElse
    transform "return"   = cJust TReturn
    transform "break"    = cJust TBreak
    transform "continue" = cJust TContinue
    transform "print"    = cJust TPrint
    transform "readint"  = cJust TReadInt
    transform _          = const Nothing

pOperator :: TParser
pOperator = pOperatorMulti <|> pOperatorSing

pOperatorMulti :: TParser
pOperatorMulti = mRmSpace pPunctuations +> transform where
    transform "==" = cJust TEq
    transform "!=" = cJust TNe
    transform "<=" = cJust TLe
    transform ">=" = cJust TGe
    transform "&&" = cJust TAnd
    transform "||" = cJust TOr
    transform _    = const Nothing

pOperatorSing ::TParser
pOperatorSing = mRmSpace pPunctuation +> transform where
    transform '+' = cJust TAdd
    transform '-' = cJust TSub
    transform '*' = cJust TMul
    transform '/' = cJust TDiv
    transform '%' = cJust TMod
    transform '<' = cJust TLt
    transform '>' = cJust TGt
    transform '!' = cJust TNot
    transform _   = const Nothing

pSinglePunc :: TParser
pSinglePunc = mRmSpace pChar >>> TSingleMark
