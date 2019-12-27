module Advent.Day1 (
day11,
day12
) where

import System.Environment
import Utils

-- usage: day1 [input-file] [1 or 2]

day11 :: String -> String
day11 input =
    show $ sum (map fuelForModule masses)
    where masses = stringInputToInts input

day12 :: String -> String
day12 input = 
    show $ sum (map (\mass -> fuelForModuleComplete mass 0) masses)
    where masses = stringInputToInts input

fuelForModule :: Int -> Int
fuelForModule mass =
    if fuelMass >= 0 then fuelMass else 0
    where fuelMass = mass `div` 3 - 2

fuelForModuleComplete :: Int -> Int -> Int
fuelForModuleComplete 0 acc = acc
fuelForModuleComplete mass acc =
    fuelForModuleComplete fuel (acc + fuel)
    where fuel = fuelForModule mass

