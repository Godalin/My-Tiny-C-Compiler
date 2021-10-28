{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module Grammar.Syntax.Expression
    ( ptExpression
    , Expression
    ) where
import           Grammar.Lexical.Basic
import           Grammar.Syntax.Basic
import           Parser.Basic
import           Parser.Conbinators

data Expression
    = ExpJst Factor
    | ExpAdd Expression Expression
    | ExpSub Expression Expression
    -- deriving Show

data Factor
    = FacJst Term
    | FacMul Factor Factor
    | FacDiv Factor Factor
    | FacMod Factor Factor
    -- deriving Show

data Term
    = TermNum {intValue :: String}
    | TermVar {varName  :: String}
    | TermExp Expression
    -- deriving Show

ptExpression :: PLex Expression
ptExpression = (ptFactor >>> formExp) <+> cIter (ptOp1 <+> (ptFactor >>> formExp))
        >>> uncurry (foldl step) where
            formExp (FacJst (TermExp e)) = e
            formExp e                    = ExpJst e
            step t1 (opToken, t2) = buildOp opToken t1 t2

ptFactor :: PLex Factor
ptFactor = (ptTerm >>> FacJst) <+> cIter (ptOp2 <+> (ptTerm >>> FacJst))
        >>> uncurry (foldl step) where
            step t1 (opToken, t2) = buildOp opToken t1 t2

ptTerm :: PLex Term
ptTerm = ptIntConstant >>> (\(TIntConstant idName) -> TermNum idName)
     <|> ptVariable  >>> (\(Var intStr) -> TermVar intStr)
     <|> cEncloseEx (ptSingleMark '(') (ptSingleMark ')') ptExpression
        >>> TermExp

class BiOperator a where
    buildOp :: Token -> (a -> a -> a)

instance BiOperator Factor where
    buildOp TMul = FacMul
    buildOp TDiv = FacDiv
    buildOp TMod = FacMod

instance BiOperator Expression where
    buildOp TAdd = ExpAdd
    buildOp TSub = ExpSub

ptOp2 :: PLex Token
ptOp2 = ptToken <=> (`elem` [TMul, TDiv, TMod])

ptOp1 :: PLex Token
ptOp1 = ptToken <=> (`elem` [TAdd, TSub])


instance Show Expression where
    show (ExpJst t)     = show t
    show (ExpAdd t1 t2) = "<" ++ show t1 ++ "+" ++ show t2 ++ ">"
    show (ExpSub t1 t2) = "<" ++ show t1 ++ "-" ++ show t2 ++ ">"

instance Show Factor where
    show (FacJst f)     = show f
    show (FacMul f1 f2) = "<" ++ show f1 ++ "*" ++ show f2 ++ ">"
    show (FacDiv f1 f2) = "<" ++ show f1 ++ "/" ++ show f2 ++ ">"
    show (FacMod f1 f2) = "<" ++ show f1 ++ "%" ++ show f2 ++ ">"

instance Show Term where
    show (TermNum intVal) = "<N:" ++ intVal ++ ">"
    show (TermVar idName) = "<V:" ++ idName ++ ">"
    show (TermExp exp)    = "<E:" ++ show exp ++ ">" -- This will never be used
