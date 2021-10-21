module Parser.Basic where

type Parser a = String -> Maybe(a, String)
