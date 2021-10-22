module Printers where

import           Parser.Basic
import           Parser.Parsers

printParser :: Show a => PSource a -> String -> String
printParser parser str = case parser str of
    Nothing      -> show ("???", str)
    Just (c, cs) -> show (show c, cs)


