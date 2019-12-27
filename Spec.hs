import Test.Hspec

import qualified Data.Map as Map

import Utils
import Advent.Day1
import qualified Advent.Day2 as Day2

main :: IO ()
main = hspec $ do
    describe "Utils.stringInputToInts" $ do
        it "constructs a list of integers from a string" $ do
            stringInputToInts "1\n2\n3\n4\n" `shouldBe` [1,2,3,4]

    describe "Advent.Day1" $ do
        describe "day11" $ do
            it "calculates fuel mass" $ do
                day11 "12" `shouldBe` "2"
                day11 "14" `shouldBe` "2"
                day11 "1969" `shouldBe` "654"
                day11 "100756" `shouldBe` "33583"
                day11 "12\n14\n1969\n100756" `shouldBe` (show $ 2 + 2 + 654 + 33583)

        describe "day12" $ do
            it "calculates complete fuel mass" $ do
                day12 "14" `shouldBe` "2"
                day12 "1969" `shouldBe` "966"
                day12 "100756" `shouldBe` "50346"
                day12 "14\n1969\n100756" `shouldBe` (show $ 2 + 966 + 50346)

    describe "Advent.Day2" $ do
        describe "inputToMap" $ do
            it "translates comma-separated input to Map" $ do
                Day2.inputToMap "1,2,3,4" `shouldBe` Map.fromList [(0,1), (1,2), (2,3), (3,4)]

        let ex1 = Day2.inputToMap "1,9,10,3,2,3,11,0,99,30,40,50"
        let ex1Res = Day2.inputToMap "3500,9,10,70,2,3,11,0,99,30,40,50"

        describe "interpretIntcode" $ do
            it "interprets intcode sequences" $ do
                Day2.interpretIntcode ex1 `shouldBe` ex1Res
            
            
