{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-
* This module defines all the expressions that we use in tiny C
* *************************************************************

* Grammar:
* <expression>      ::= <math_expression>|<bool_expression>|<comp_expression>
* <math_expression> ::= <->?<term>((<+>|<->)<term>)*
* <term>            ::= <factor>((<*>|</>|<%>)<factor>)*
* <factor>          ::= <num>|<var>|(<(><expression><)>)
* <comp_expression> ::= <expression>(<==>|<!=>|<<>|<>>|<<=>|<>=>)<expression>
* <bool_expression> ::= <bool_term>(<&&>|<||>)<bool_term>
* <bool_term>       ::= <!><factor>|<expression>
-}

-- module Grammar.Syntax.Expression where
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
    show (ENot e)      = "<! " ++ show e ++ ">"
    show (EAnd f1 f2)  = "<" ++ show f1 ++ " & " ++ show f2 ++ ">"
    show (EOr  f1 f2)  = "<" ++ show f1 ++ " | " ++ show f2 ++ ">"


ptExpression :: PLex Expression
ptExpression =
        ptMathExpression
    <|> ptBoolExpression
    <|> ptCompExpression


ptMathExpression :: PLex Expression
ptMathExpression = (
        ptOpNeg <-+> ptTerm >>> ENeg
    <|> ptTerm)
    <+> cIter (ptOpAdds <+> ptTerm)
    >>> uncurry (foldl step) where
        step t1 (opToken, t2) = buildBiOp opToken t1 t2

ptTerm :: PLex Expression
ptTerm =
        ptFactor <+> cIter (ptOpMuls <+> ptFactor)
    >>> uncurry (foldl step) where
        step t1 (opToken, t2) = buildBiOp opToken t1 t2

ptFactor :: PLex Expression
ptFactor =
        ptIntConstant >>> (\(TIntConstant idName) -> ENum idName)
    <|> ptVariable    >>> (\(Var intStr)          -> EVar intStr)
    <|> cEncloseEx (ptSingleMark '(') (ptSingleMark ')') ptExpression


ptBoolExpression :: PLex Expression
ptBoolExpression =
        ptBoolTerm <+> cIter (ptOpAnds <+> ptBoolTerm)
    >>> uncurry (foldl step) where
        step t1 (opToken, t2) = buildBiOp opToken t1 t2


ptBoolTerm :: PLex Expression
ptBoolTerm =
        ptOpNot <-+> ptFactor >>> ENot
    <|> ptExpression


ptCompExpression :: PLex Expression
ptCompExpression =
        ptExpression <+> ptOpComps <+> ptExpression
    >>> (\((t1, op), t2) -> buildBiOp op t1 t2)

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
    
ptOpMuls :: PLex Token
ptOpMuls = ptToken <=> (`elem` [TMul, TDiv, TMod])

ptOpAdds :: PLex Token
ptOpAdds = ptToken <=> (`elem` [TAdd, TSub])

ptOpNeg :: PLex Token
ptOpNeg = ptToken <=> (== TSub)

ptOpComps :: PLex Token
ptOpComps = ptToken <=> (`elem` [TEq, TNe, TGt, TLt, TGe, TLe])

ptOpAnds :: PLex Token
ptOpAnds = ptToken <=> (`elem` [TAnd, TOr])

ptOpNot :: PLex Token
ptOpNot = ptToken <=> (== TNot)
