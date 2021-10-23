module Grammar.Lexical.Basic where

import           Parser.Basic
import           Parser.Conbinators
import           Parser.Parsers

type TParser = PSource Token


data Token
    = TAdd | TSub | TMul | TDiv | TMod
    | TEq  | TNe  | TLe  | TGe  | TGt  | TLt
    | TAnd | TOr  | TNot
    | TIdentifier     {idName   :: String}
    | TIntConstant    {intValue :: String}
    | TStringConstant {strValue :: String}
    | TVoid    | TInt
    | TWhile   | TIf      | TElse
    | TReturn  | TBreak   | TContinue
    | TPrint   | TReadInt
    | TSingleMark Char
    deriving (Show, Eq)
