module Grammar.Syntax.Syntax where

import           Grammar.Lexical.Basic
import           Grammar.Syntax.Basic
import           Grammar.Syntax.Expression
import           Parser.Conbinators

data Statement
    = SAssignment       Var         Expression
    | SIfStatement      Expression  [Statement]
    | SWhileStatement   Expression  [Statement]
    | SFunctionCall     Fun         [Expression]
    | SFunDefinition    Fun         ReturnType    [Var]           [Statement]
    | SVarDeclaration   [Var]
    deriving Show

ptStatement :: PLex Statement
ptStatement = ptAssignment
            <|> ptIfStatement
            <|> ptWhileStatement
            <|> ptFunctionCall

-- TODO useless paramater receiving
-- TODO useless when there is no variable declaration
ptFunDefinition :: PLex Statement
ptFunDefinition = (ptTypeInt >>> const RtInt <|> ptTypeVoid >>> const RtVoid )
            <+> ptFunction
            <+> mParenthesis (cIter ptVariable)
            <+> mBlock (ptVarDeclaration <+> cIter ptVarDeclaration >>> uncurry (:))
            >>> (\(((rtType, fName), vars), body) -> SFunDefinition fName rtType vars body)

ptVarDeclaration :: PLex Statement
ptVarDeclaration = cIter ptVarDeclarationLn >>> foldl step [] >>> SVarDeclaration where
    step vs1 (SVarDeclaration vs2) = vs1 ++ vs2
    step _ _                       = undefined

ptVarDeclarationLn :: PLex Statement
ptVarDeclarationLn = mSimple (ptTypeInt <-+> ptVariable
                <+> cIter (ptSingleMark ',' <-+> ptVariable)
                >>> uncurry (:)
                >>> SVarDeclaration)

ptAssignment :: PLex Statement
ptAssignment = mSimple (ptVariable
                <+-> ptSingleMark '='
                <+>  ptExpression)
                >>>  uncurry SAssignment

ptIfStatement :: PLex Statement
ptIfStatement = (ptToken <=> (== TIf))
                <-+> mParenthesis ptExpression
                <+>  mBlock (cIter ptStatement)
                >>>  uncurry SIfStatement

ptWhileStatement :: PLex Statement
ptWhileStatement = (ptToken <=> (== TWhile))
                <-+> mParenthesis ptExpression
                <+>  mBlock (cIter ptStatement)
                >>>  uncurry SWhileStatement

ptFunctionCall :: PLex Statement
ptFunctionCall = ptFunction <+> mParenthesis (cIter ptExpression)
                >>> uncurry SFunctionCall

mSimple :: PLex a -> PLex a
mSimple = cEndEx $ ptSingleMark ';'

mBlock :: PLex a -> PLex a
mBlock = cEncloseEx (ptSingleMark '{') (ptSingleMark '}')

mParenthesis :: PLex a -> PLex a
mParenthesis = cEncloseEx (ptSingleMark '(') (ptSingleMark ')')
