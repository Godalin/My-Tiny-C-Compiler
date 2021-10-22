module Parser.Basic where

type Parser s a = [s] -> Maybe(a, [s])

type Predicate a = (a -> Bool)
type Transform a b = (a -> b)

-- For analysis of char stream
type PSource a = Parser Char a
