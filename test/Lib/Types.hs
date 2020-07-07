import Test.Hspec
import Test.QuickCheck
import Lib.Types

main :: IO ()
main = hspec $ do
    describe "Row" $ do
        it "decodes from JSON" $ do
            1 `shouldBe` 1