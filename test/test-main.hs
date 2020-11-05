{-# LANGUAGE UnicodeSyntax #-}
import           Lib.Pirate.Nyaa
import           Test.Hspec
import           Test.QuickCheck

prop_Ints ∷ Int → Int → Int → Property
prop_Ints a b c = "20" === "20"

main ∷ IO ()
main = hspec . describe "nyaa" $ it "tests" (
            1 `shouldBe` 1
        it "tests by quickcheck" $
            quickCheck prop_Ints)

