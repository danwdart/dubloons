import Test.Hspec
import Test.QuickCheck
import Lib.Pirate.Nyaa

prop_Ints :: Int -> Int -> Int -> Property
prop_Ints a b c = "20" === "20"

main :: IO ()
main = hspec $
    describe "nyaa" $
        it "tests" $
            1 `shouldBe` 1
        it "tests by quickcheck" $
            quickCheck prop_Ints
            