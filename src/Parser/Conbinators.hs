module Parser.Conbinators where

import           Parser.Basic

type Predicate a = (a -> Bool)
type Transform a b = (a -> b)

infixl 7 <=>
(<=>) :: Parser a -> Predicate a -> Parser a
(parserA <=> predicate) input = case parserA input of
    Nothing -> Nothing
    result@(Just (resultA, rest))
        | predicate resultA -> result
        | otherwise         -> Nothing

infixl 6 <+>
(<+>) :: Parser a -> Parser b -> Parser (a, b)
(parserA <+> parserB) input = case parserA input of
    Nothing               -> Nothing
    Just (resultA, restA) -> case parserB restA of
        Nothing               -> Nothing
        Just (resultB, restB) -> Just ((resultA, resultB), restB)

infixl 6 <+->
(<+->) :: Parser a -> Parser b -> Parser a
(parserA <+-> parserB) input = case (parserA <+> parserB) input of
    Nothing                         -> Nothing
    Just ((resultA, resultB), rest) -> Just (resultA, rest)

infixl 6 <-+>
(<-+>) :: Parser a -> Parser b -> Parser b
(parserA <-+> parserB) input = case (parserA <+> parserB) input of
    Nothing                         -> Nothing
    Just ((resultA, resultB), rest) -> Just (resultB, rest)

infixl 5 +>
(+>) :: Parser a -> Transform a (Parser b) -> Parser b
(parserA +> transform) input = case parserA input of
    Nothing               -> Nothing
    Just (resultA, restA) -> transform resultA restA

infix 4 >>>
(>>>) :: Parser a -> Transform a b -> Parser b
(parserA >>> transform) input = case parserA input of
    Nothing               -> Nothing
    Just (resultA, restA) -> Just (transform resultA, restA)

infix 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a
(parserA <|> parserB) input = case parserA input of
    Nothing -> parserB input
    resultA -> resultA
