module Grammar.Syntax.Syntax where

import           Grammar.Syntax.Basic
import           Grammar.Syntax.Expression
import           Parser.Conbinators

data Statement
    = SAssignment Var Expression
    | SIfStatement
    | SWhileStatement
    | SFunctionCall
    | SDeclaration [Var]
    deriving Show

ptDeclaration :: PLex Statement
ptDeclaration = cIter ptDeclarationLn >>> foldl step [] >>> SDeclaration where
    step vs1 (SDeclaration vs2) = vs1 ++ vs2
    step _ _                    = undefined

ptDeclarationLn :: PLex Statement
ptDeclarationLn = mSimple (ptTypeInt <-+> ptVariable
                <+> cIter (ptSingleMark ',' <-+> ptVariable)
                >>> uncurry (:)
                >>> SDeclaration)

ptAssignment :: PLex Statement
ptAssignment = mSimple (ptVariable
                <+-> ptSingleMark '='
                <+> ptExpression)
                >>> uncurry SAssignment

mSimple :: PLex a -> PLex a
mSimple = cEndEx $ ptSingleMark ';'

mBlock :: PLex a -> PLex a
mBlock = cEncloseEx (ptSingleMark '{') (ptSingleMark '}')
