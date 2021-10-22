import           Control.Monad
import           Grammar.Lexical.Lexical (genTokenStream)
import           Test.Hspec

main :: IO ()
main = do
    source <- readFile "./example.c"
    mapM_ print (genTokenStream source)
