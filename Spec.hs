import Test.Hspec

import Utils

main :: IO ()
main = hspec $ do
    describe "Utils.stringInputToInts" $ do
        it "constructs a list of integers from a string" $ do
            stringInputToInts "1\n2\n3\n4\n" `shouldBe` [1,2,3,4]
