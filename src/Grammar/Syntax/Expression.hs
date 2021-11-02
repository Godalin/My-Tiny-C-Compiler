{-# LANGUAGE MultiParamTypeClasses #-}
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
* simple maths expressions
* <Expression> ::= <Expression> ([+-] <Expression>)*
* <Factor>     ::= <Term>       ([*/] <Term>)*
* <Term>       ::= <Num> | <Var> | <Expression>
-}

class Operator2x a b where
    build2Op :: Token -> (a -> a -> b)

class Operator1x a b where
    build1Op :: Token -> (a -> b)

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
            step t1 (opToken, t2) = build2Op opToken t1 t2

ptFactor :: PLex Factor
ptFactor = (ptTerm >>> FacJst) <+> cIter (ptOp2 <+> (ptTerm >>> FacJst))
        >>> uncurry (foldl step) where
            step t1 (opToken, t2) = build2Op opToken t1 t2

ptTerm :: PLex Term
ptTerm = ptIntConstant >>> (\(TIntConstant idName) -> TermNum idName)
     <|> ptVariable  >>> (\(Var intStr) -> TermVar intStr)
     <|> cEncloseEx (ptSingleMark '(') (ptSingleMark ')') ptExpression
        >>> TermExp


instance Operator2x Factor Factor where
    build2Op TMul = FacMul
    build2Op TDiv = FacDiv
    build2Op TMod = FacMod

instance Operator2x Expression Expression where
    build2Op TAdd = ExpAdd
    build2Op TSub = ExpSub

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
    show (TermNum intVal) = "<N:" ++ intVal   ++ ">"
    show (TermVar idName) = "<V:" ++ idName   ++ ">"
    show (TermExp exp)    = "<E:" ++ show exp ++ ">" -- This will never be used



data BExpression
    = BJst Expression
    | BNot BExpression
    | BAnd BExpression BExpression
    | BOr  BExpression BExpression
    deriving Show

data BComparison
    = Expression `BCEQ` Expression
    | Expression `BCNE` Expression
    | Expression `BCGT` Expression
    | Expression `BCLT` Expression
    | Expression `BCGE` Expression
    | Expression `BCLE` Expression
    deriving Show

instance Operator2x BExpression BExpression where
    build2Op TAnd = BAnd
    build2Op TOr  = BOr

ptOpBool2 :: PLex Token
ptOpBool2 = ptToken <=> (`elem` [TAnd, TOr])

ptBNot :: PLex Token
ptBNot = ptToken <=> (== TNot)

instance Operator2x Expression BComparison where
    build2Op TEq = BCEQ
    build2Op TNe = BCNE
    build2Op TGt = BCGT
    build2Op TLt = BCLT
    build2Op TGe = BCGE
    build2Op TLe = BCLE

ptOpComparison :: PLex Token
ptOpComparison = ptToken <=> (`elem` [TEq, TNe, TGt, TLt, TGe, TLe])

ptComparison :: PLex BComparison
ptComparison = ptExpression <+> ptOpComparison <+> ptExpression
                >>> (\((l, op), r) -> build2Op op l r)
