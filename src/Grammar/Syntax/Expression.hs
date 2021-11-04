{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module Grammar.Syntax.Expression
    -- ( ptExpression
    -- , Expression
    -- ) where
where
import           Grammar.Lexical.Basic
import           Grammar.Syntax.Basic
import           Parser.Conbinators

{-
* This module defines all the expressions that we use in tiny C
* In this version, I parse according to operator precedence
* *************************************************************
* level0 : <num>|       <var>|              <(><expression><)>
* level1 :              (<->|<!>)           <expression>
* level2 : <expression> (<*>|</>|<%>)       <expression>
* level3 : <expression> (<+>|<->)           <expression>
* level4 : <expression> (<>>|<>=>|<<>|<<=>) <expression>
* level5 : <expression> (<==>|<!=>)         <expression>
* level6 : <expression> <&&>                <expression>
* level7 : <expression> <||>                <expression>
-}

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
    show (ENum intVal) = "<N:" ++ intVal  ++ ">"
    show (EVar idName) = "<V:" ++ idName  ++ ">"
    show (ENeg e)      = "<-"  ++ show e  ++ ">"
    show (EAdd t1 t2)  = "<"   ++ show t1 ++ " + "  ++ show t2 ++ ">"
    show (ESub t1 t2)  = "<"   ++ show t1 ++ " - "  ++ show t2 ++ ">"
    show (EMul f1 f2)  = "<"   ++ show f1 ++ " * "  ++ show f2 ++ ">"
    show (EDiv f1 f2)  = "<"   ++ show f1 ++ " / "  ++ show f2 ++ ">"
    show (EMod f1 f2)  = "<"   ++ show f1 ++ " % "  ++ show f2 ++ ">"
    show (ENot e)      = "<!"  ++ show e  ++ ">"
    show (EAnd f1 f2)  = "<"   ++ show f1 ++ " & "  ++ show f2 ++ ">"
    show (EOr  f1 f2)  = "<"   ++ show f1 ++ " | "  ++ show f2 ++ ">"
    show (EEq  e1 e2)  = "<"   ++ show e1 ++ " == " ++ show e2 ++ ">"
    show (ENe  e1 e2)  = "<"   ++ show e1 ++ " != " ++ show e2 ++ ">"
    show (EGt  e1 e2)  = "<"   ++ show e1 ++ " > "  ++ show e2 ++ ">"
    show (EGe  e1 e2)  = "<"   ++ show e1 ++ " >= " ++ show e2 ++ ">"
    show (ELt  e1 e2)  = "<"   ++ show e1 ++ " < "  ++ show e2 ++ ">"
    show (ELe  e1 e2)  = "<"   ++ show e1 ++ " <= " ++ show e2 ++ ">"


ptExpression :: PLex Expression
ptExpression = ptL07

ptL00 :: PLex Expression
ptL00 =
        ptIntConstant >>> (\(TIntConstant idName) -> ENum idName)
    <|> ptVariable    >>> (\(Var intStr)          -> EVar intStr)
    <|> cEncloseEx (ptSingleMark '(') (ptSingleMark ')') ptExpression


ptL01 :: PLex Expression
ptL01 = ptUnary (ptToken <=> (`elem` [TSub, TNot])) ptL00 <|> ptL00

ptL02 :: PLex Expression
ptL02 = ptBinary (ptToken <=> (`elem` [TMul, TDiv, TMod])) ptL01

ptL03 :: PLex Expression
ptL03 = ptBinary (ptToken <=> (`elem` [TAdd, TSub])) ptL02

ptL04 :: PLex Expression
ptL04 = ptBinary (ptToken <=> (`elem` [TGt, TGe, TLt, TLe])) ptL03

ptL05 :: PLex Expression
ptL05 = ptBinary (ptToken <=> (`elem` [TEq, TNe])) ptL04

ptL06 :: PLex Expression
ptL06 = ptBinary (ptToken <=> (== TAnd)) ptL05

ptL07 :: PLex Expression
ptL07 = ptBinary (ptToken <=> (== TOr)) ptL06


ptUnary :: PLex Token      -- ^ operator token
        -> PLex Expression -- ^ expression of some level
        -> PLex Expression -- ^ parser for the next level
ptUnary ptOperator ptLl =
        ptOperator <+> ptLl >>> uncurry buildUnOp


ptBinary :: PLex Token      -- ^ operator token
         -> PLex Expression -- ^ expression of some level
         -> PLex Expression -- ^ parser for series of low level with operators
ptBinary ptOperator ptLl =
        ptLl <+> cIter (ptOperator <+> ptLl) >>> uncurry (foldl step) where
            step e1 (op, e2) = buildBiOp op e1 e2


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
