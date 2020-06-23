import Test.Hspec
import Test.QuickCheck
import Lib.Pirate.Nyaa

main :: IO ()
main = quickCheck $ do
    describe "nyaa" $ do
        1 `shouldBe` 1