module Parser.Conbinators where

import           Parser.Basic

infixl 7 <=>
(<=>) :: Parser s a -> Predicate a -> Parser s a
(parserA <=> predicate) input = case parserA input of
    Nothing -> Nothing
    result@(Just (resultA, rest))
        | predicate resultA -> result
        | otherwise         -> Nothing

infixl 6 <+>
(<+>) :: Parser s a -> Parser s b -> Parser s (a, b)
(parserA <+> parserB) input = case parserA input of
    Nothing               -> Nothing
    Just (resultA, restA) -> case parserB restA of
        Nothing               -> Nothing
        Just (resultB, restB) -> Just ((resultA, resultB), restB)

infixl 6 <+->
(<+->) :: Parser s a -> Parser s b -> Parser s a
(parserA <+-> parserB) input = case (parserA <+> parserB) input of
    Nothing                         -> Nothing
    Just ((resultA, resultB), rest) -> Just (resultA, rest)

infixl 6 <-+>
(<-+>) :: Parser s a -> Parser s b -> Parser s b
(parserA <-+> parserB) input = case (parserA <+> parserB) input of
    Nothing                         -> Nothing
    Just ((resultA, resultB), rest) -> Just (resultB, rest)

infixl 5 +>
(+>) :: Parser s a -> Transform a (Parser s b) -> Parser s b
(parserA +> transform) input = case parserA input of
    Nothing               -> Nothing
    Just (resultA, restA) -> transform resultA restA

infixl 4 >>>
(>>>) :: Parser s a -> Transform a b -> Parser s b
(parserA >>> transform) input = case parserA input of
    Nothing               -> Nothing
    Just (resultA, restA) -> Just (transform resultA, restA)

infixl 3 <|>
(<|>) :: Parser s a -> Parser s a -> Parser s a
(parserA <|> parserB) input = case parserA input of
    Nothing -> parserB input
    resultA -> resultA

cJust :: a -> Parser s a
cJust x input = Just(x, input)

cIter :: Parser s a -> Parser s [a]
cIter parserA = parserA <+> cIter parserA >>> uncurry (:)
           <|> cJust []

cIterSafe :: Parser s a -> Parser s [a]
cIterSafe parserA input = case cIter parserA input of
    Just ([], _) -> Nothing
    result       -> result
