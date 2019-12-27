module Advent.Day2 (
day21,
inputToMap,
interpretIntcode
) where

import Debug.Trace

import Data.List.Split
import qualified Data.Map as Map

day21 :: String -> String
day21 input = 
    let inCode = Map.insert 1 12 .
                Map.insert 2 2 $ inputToMap input
        result = interpretIntcode inCode
    in show $ result Map.! 0

inputToMap :: String -> Map.Map Int Int
inputToMap input =
    let inList = splitOn "," input
        assocList = zip [0..(length inList - 1)] $ map read inList
    in Map.fromList assocList

interpretIntcode :: Map.Map Int Int -> Map.Map Int Int
interpretIntcode intcode =
    interpretIntcodeAux intcode 0

interpretIntcodeAux :: Map.Map Int Int -> Int -> Map.Map Int Int
--interpretIntcodeAux intcode pointer | trace ("pointer = " ++ show pointer ++ " intcode = " ++ show intcode) False = undefined
interpretIntcodeAux intcode pointer
    | Map.notMember pointer intcode = error "you dun goofed"
    | inst == 99    = intcode
    | inst == 1     = interpretIntcodeAux (interpretFn (+)) (pointer + 4)
    | inst == 2     = interpretIntcodeAux (interpretFn (*)) (pointer + 4)
    | otherwise     = error "you double goofed"
    where inst = intcode Map.! pointer
          param1 = intcode Map.! (intcode Map.! (pointer + 1)) -- Fetch the first operand
          param2 = intcode Map.! (intcode Map.! (pointer + 2)) -- Fetch the second operand
          res = intcode Map.! (pointer + 3) -- The location to place the result
          interpretFn = applyInstruction param1 param2 res intcode


applyInstruction :: Int -> Int -> Int -> Map.Map Int Int -> (Int -> Int -> Int) -> Map.Map Int Int
--applyInstruction param1 param2 res intcode fun | trace ("function applied to " ++ show param1 ++ " " ++ show param2 ++ " stored in res " ++ show res) False = undefined
applyInstruction param1 param2 res intcode fun =
    Map.insert res (fun param1 param2) intcode
