module Grammar.Syntax.Syntax where

import           Grammar.Lexical.Basic
import           Grammar.Syntax.Basic
import           Parser.Basic
import           Parser.Conbinators



ptDeclaration :: PLex Statement
ptDeclaration = mSimple(ptTypeInt <-+> ptVariable
                <+> cIter (ptSingleMark ',' <-+> ptVariable)
                >>> uncurry (:)
                >>> SDeclaration)

ptAssignment :: PLex Statement
ptAssignment = undefined

mSimple :: PLex a -> PLex a
mSimple = cEndEx $ ptSingleMark ';'

mBlock :: PLex a -> PLex a
mBlock = cEncloseEx (ptSingleMark '{') (ptSingleMark '}')
