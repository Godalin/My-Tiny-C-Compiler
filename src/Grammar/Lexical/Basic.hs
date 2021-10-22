module Grammar.Lexical.Basic where

import           Parser.Conbinators
import           Parser.Parsers


data Token
    = TAdd | TSub | TMul | TDiv | TMod
    | TEq  | TNe  | TLe  | TGe  | TGt  | TLt
    | TAnd | TOr  | TNot
    | TIdentifier     {idName :: String}
    | TIntConstant    {intValue :: Int}
    | TStringConstant {stringValue :: String}
    | TAssign
    | TVoid    | TInt
    | TWhile   | TIf      | TElse
    | TReturn  | TBreak   | TContinue
    | TPrint   | TReadInt
    | TSinglePunc Char
    deriving Show
