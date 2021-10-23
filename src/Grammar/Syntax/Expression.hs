{-# LANGUAGE MultiParamTypeClasses #-}
module Grammar.Syntax.Expression where
import           Grammar.Lexical.Basic
import           Grammar.Syntax.Basic
import           Parser.Basic
import           Parser.Conbinators

data Expression
    = ExpJst Term
    | ExpAdd Term Term
    | ExpSub Term Term

data Term
    = TermJst Factor
    | TermMul Factor Factor
    | TermDiv Factor Factor
    | TermMod Factor Factor

data Factor
    = FacNum {intValue :: String}
    | FacVar {varName  :: String}

ptExpression :: PLex Expression
ptExpression = undefined

ptTerm :: PLex Term
ptTerm =
        ptFactor <+> ptOp2 <+> ptFactor >>> buildOp
    <|> ptFactor >>> TermJst

ptFactor :: PLex Factor
ptFactor =
        ptVariable    >>> (\(TIdentifier  value) -> FacVar value)
    <|> ptIntConstant >>> (\(TIntConstant value) -> FacNum value)

class BiOperator a b where
    buildOp :: Transform ((a, Token), a) b

instance BiOperator Factor Term where
    buildOp = \((f1, op), f2) -> build op f1 f2 where
        build TMul = TermMul
        build TDiv = TermDiv
        build TMod = TermMod
        build _    = undefined

ptOp2 :: PLex Token
ptOp2 = ptToken <=> (`elem` [TMul, TDiv, TMod])

ptOp1 :: PLex Token
ptOp1 = ptToken <=> (`elem` [TAdd, TSub])


instance Show Expression where
    show (ExpJst t)     = show t
    show (ExpAdd t1 t2) = "<" ++ show t1 ++ "+" ++ show t2 ++ ">"
    show (ExpSub t1 t2) = "<" ++ show t1 ++ "-" ++ show t2 ++ ">"

instance Show Term where
    show (TermJst f)     = show f
    show (TermMul f1 f2) = "<" ++ show f1 ++ "*" ++ show f2 ++ ">"
    show (TermDiv f1 f2) = "<" ++ show f1 ++ "/" ++ show f2 ++ ">"
    show (TermMod f1 f2) = "<" ++ show f1 ++ "%" ++ show f2 ++ ">"

instance Show Factor where
    show (FacNum intVal) = "<Num:" ++ intVal ++ ">"
    show (FacVar idName) = "<Var:" ++ idName ++ ">"
