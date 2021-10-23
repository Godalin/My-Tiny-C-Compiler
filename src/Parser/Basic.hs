module Parser.Basic where

type Parser s a = [s] -> Maybe (a, [s])

bParser :: Parser s s
bParser []     = Nothing
bParser (x:xs) = Just (x, xs)

bNot :: Parser s s -> Parser s s
bNot parserA []           = Nothing
bNot parserA input@(x:xs) = case parserA input of
    Nothing -> Just (x, xs)
    Just _  -> Nothing

type Predicate a = (a -> Bool)
type Transform a b = (a -> b)

-- For analysis of char stream
type PSource a = Parser Char a
