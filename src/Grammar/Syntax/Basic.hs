module Grammar.Syntax.Basic where

import           Grammar.Lexical.Basic
import           Grammar.Lexical.Printer
import           Parser.Basic
import           Parser.Conbinators

type PLex a = Parser Token a

data Statement
    = SAssignment Token Token
    | SIfStatement
    | SWhileStatement
    | SFunctionCall
    | SDeclaration [Token]
    deriving Show

ptToken :: PLex Token
ptToken = bParser

ptSingleMark :: Char -> PLex Token
ptSingleMark c = ptToken <=> (== TSingleMark c)

ptTypeInt :: PLex Token
ptTypeInt = ptToken <=> (==TInt)

ptVariable :: PLex Token
ptVariable = ptToken <=> isIdentifier
    where isIdentifier (TIdentifier _) = True
          isIdentifier _               = False

ptIntConstant :: PLex Token
ptIntConstant = ptToken <=> isNumber
    where isNumber (TIntConstant _) = True
          isNumber _                = False
