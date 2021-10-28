module Grammar.Syntax.Basic where

import           Grammar.Lexical.Basic
import           Parser.Basic
import           Parser.Conbinators

type PLex a = Parser Token a


newtype Var = Var {varName :: String}
instance Show Var where
    show v = "<V:" ++ varName v ++ ">"


newtype Fun = Fun {funName :: String}
instance Show Fun where
    show f = "<$" ++ funName f ++ ">"

data ReturnType = RtInt | RtVoid
instance Show ReturnType where
    show RtInt  = "<Rt.I>"
    show RtVoid = "<Rt.V>"

ptToken :: PLex Token
ptToken = bParser

ptSingleMark :: Char -> PLex Token
ptSingleMark c = ptToken <=> (== TSingleMark c)

ptTypeInt :: PLex Token
ptTypeInt = ptToken <=> (== TInt)

ptTypeVoid :: PLex Token
ptTypeVoid = ptToken <=> (== TVoid)

ptVariable :: PLex Var
ptVariable = ptToken <=> isIdentifier
            >>> (\(TIdentifier name) -> Var name)

ptFunction :: PLex Fun
ptFunction = ptToken <=> isIdentifier
            >>> (\(TIdentifier name) -> Fun name)

ptIntConstant :: PLex Token
ptIntConstant = ptToken <=> isNumber

isIdentifier :: Token -> Bool
isIdentifier (TIdentifier _) = True
isIdentifier _               = False

isNumber :: Token -> Bool
isNumber (TIntConstant _) = True
isNumber _                = False
