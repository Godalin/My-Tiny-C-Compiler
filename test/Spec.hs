import           Parser.BasicParsers
import           Parser.Conbinators
import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Tests for Conbinators" $ do
        it "<=>" $ do
            someFunc `shouldBe` "aaa"

someFunc :: String
someFunc = "aaa"
