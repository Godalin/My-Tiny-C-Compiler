module Grammar.Syntax.Basic where

import           Grammar.Lexical.Basic
import           Parser.Basic
import           Parser.Conbinators

type PLex a = Parser Token a

newtype Var = Var {varName :: String}

instance Show Var where
    show a = "<V:" ++ varName a ++ ">"

ptToken :: PLex Token
ptToken = bParser

ptSingleMark :: Char -> PLex Token
ptSingleMark c = ptToken <=> (== TSingleMark c)

ptTypeInt :: PLex Token
ptTypeInt = ptToken <=> (== TInt)

ptVariable :: PLex Var
ptVariable = ptToken <=> isIdentifier >>> (\(TIdentifier name) -> Var name)
    where isIdentifier (TIdentifier _) = True
          isIdentifier _               = False

ptIntConstant :: PLex Token
ptIntConstant = ptToken <=> isNumber
    where isNumber (TIntConstant _) = True
          isNumber _                = False
