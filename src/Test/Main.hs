module Test.Main where


tests = do
    return ()

main :: IO ()
main = quickCheck tests