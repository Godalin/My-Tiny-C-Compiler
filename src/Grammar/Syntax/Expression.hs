{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module Grammar.Syntax.Expression where
import           Grammar.Lexical.Basic
import           Grammar.Syntax.Basic
import           Parser.Conbinators

data Expression
    = ENum {intValue :: String}
    | EVar {varName  :: String}

    | ENeg Expression
    | Expression `EAdd` Expression
    | Expression `ESub` Expression

    | Expression `EMul` Expression
    | Expression `EDiv` Expression
    | Expression `EMod` Expression

    | ENot Expression
    | Expression `EAnd` Expression
    | Expression `EOr`  Expression

    | Expression `EEq`  Expression
    | Expression `ENe`  Expression
    | Expression `EGt`  Expression
    | Expression `ELt`  Expression
    | Expression `EGe`  Expression
    | Expression `ELe`  Expression

instance Show Expression where
    show (ENum intVal) = "<N:" ++ intVal ++ ">"
    show (EVar idName) = "<V:" ++ idName ++ ">"
    show (ENeg e)      = "<- " ++ show e ++ ">"
    show (EAdd t1 t2)  = "<" ++ show t1 ++ " + " ++ show t2 ++ ">"
    show (ESub t1 t2)  = "<" ++ show t1 ++ " - " ++ show t2 ++ ">"
    show (EMul f1 f2)  = "<" ++ show f1 ++ " * " ++ show f2 ++ ">"
    show (EDiv f1 f2)  = "<" ++ show f1 ++ " / " ++ show f2 ++ ">"
    show (EMod f1 f2)  = "<" ++ show f1 ++ " % " ++ show f2 ++ ">"


ptExpression :: PLex Expression
ptExpression = undefined


ptMathExpression :: PLex Expression
ptMathExpression =
        (ptOpNeg <-+> ptFactor >>> ENeg
    <|> ptFactor)
    <+> cIter (ptOpAdds <+> ptFactor)
    >>> uncurry (foldl step) where
        step t1 (opToken, t2) = buildBiOp opToken t1 t2

ptFactor :: PLex Expression
ptFactor =
        ptTerm <+> cIter (ptOpMuls <+> ptTerm)
    >>> uncurry (foldl step) where
        step t1 (opToken, t2) = buildBiOp opToken t1 t2

ptTerm :: PLex Expression
ptTerm =
        ptIntConstant >>> (\(TIntConstant idName) -> ENum idName)
    <|> ptVariable    >>> (\(Var intStr)          -> EVar intStr)
    <|> cEncloseEx (ptSingleMark '(') (ptSingleMark ')') ptMathExpression


buildBiOp :: Token -> (Expression -> Expression -> Expression)
buildBiOp t = case t of
    TAdd -> EAdd
    TSub -> ESub

    TMul -> EMul
    TDiv -> EDiv
    TMod -> EMod

    TAnd -> EAnd
    TOr  -> EOr

    TEq  -> EEq
    TNe  -> ENe
    TGt  -> EGt
    TLt  -> ELt
    TGe  -> EGe
    TLe  -> ELe

buildUnOp :: Token -> (Expression -> Expression)
buildUnOp t = case t of
    TSub -> ENeg
    TNot -> ENot


ptOpMuls :: PLex Token
ptOpMuls = ptToken <=> (`elem` [TMul, TDiv, TMod])

ptOpAdds :: PLex Token
ptOpAdds = ptToken <=> (`elem` [TAdd, TSub])

ptOpNeg :: PLex Token
ptOpNeg = ptToken <=> (== TSub)
