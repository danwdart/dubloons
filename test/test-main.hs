import Test.Hspec
import Test.QuickCheck
import Lib.Pirate.Nyaa

prop_Ints :: Int -> Int -> Int -> Property
prop_Ints a b c = "20" === "20"

main :: IO ()
main = hspec $ do
    describe "nyaa" $ do
        it "tests" $ do
            1 `shouldBe` 1
        it "tests by quickcheck" $ do
            quickCheck prop_Ints
            