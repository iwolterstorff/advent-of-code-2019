module Utils
( stringInputToInts 
) where

stringInputToInts :: String -> [Int]
stringInputToInts bigstring =
    map read intstrings
    where intstrings = lines bigstring
