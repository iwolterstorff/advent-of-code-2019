module Advent.Day2 (
day21
) where

import Data.List.Split

day21 :: String -> String
day21 input =
    show $ head (interpretIntcode (replaceNth (replaceNth code 1 12) 2 2) 0)
    where code = map read $ splitOn "," input

interpretIntcode :: [Int] -> Int -> [Int]
interpretIntcode code pointer
    | opcode == 99 = code
    | opcode == 1 = intcodeOp (+) code param1 param2 resPnt
    | opcode == 2 = intcodeOp (*) code param1 param2 resPnt
    where opcode = code !! pointer
          param1 = code !! (pointer + 1)
          param2 = code !! (pointer + 2)
          resPnt = code !! (pointer + 3)

intcodeOp :: (Int -> Int -> Int) -> [Int] -> Int -> Int -> Int -> [Int]
intcodeOp fun code param1 param2 resPnt =
    replaceNth code resPnt (fun param1 param2)

replaceNth :: [a] -> Int -> a -> [a]
replaceNth xs ind elem =
    let (x,_:ys) = splitAt ind xs
    in x ++ (elem : ys)
