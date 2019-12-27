{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Map as Map
import System.Environment

import Advent.Day1
import Advent.Day2
import Advent.Day3

-- usage: ./aoc [day-num 1-25] [prob-num 1-2]

functionMap :: Map.Map (Int, Int) (String -> String) =
   Map.insert (1, 1) day11 .
   Map.insert (1, 2) day12 .
   Map.insert (2, 1) day21 .
   Map.insert (2, 2) day22 .
   Map.insert (3, 1) day31 $ Map.empty

main :: IO ()
main = do
    [dayNumStr, probNumStr] <- getArgs
    fileContents <- readFile ("data/day" ++ dayNumStr ++ "-in.txt")
    let dayNum :: Int = read dayNumStr
    let probNum :: Int = read probNumStr
    putStrLn $ (functionMap Map.! (dayNum, probNum)) fileContents
    
