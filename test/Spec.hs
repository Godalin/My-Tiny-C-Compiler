import           Grammar.Lexical.Lexical (genTokenStream)
import           Grammar.Syntax.Syntax   (genProgram)
import           Test.Hspec

main :: IO ()
main = do
    -- source <- readFile "./example.c"
    let source = "int main () { int a; int b, c; if (a) {a=c; while (c+d) { a=b;b=a; }}} int aaa(){}"
    let ts = genTokenStream source
    let ss = genProgram ts
    mapM_ print ss
