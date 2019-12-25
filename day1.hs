import System.Environment

-- usage: day1 [input-file] [1 or 2]

main :: IO ()
main = do
    [fileName, probNum] <- getArgs
    fileContents <- readFile fileName
    case probNum of
        "1" -> print (prob1 fileContents)
        _ -> error "Invalid probnum"

prob1 :: String -> Int
prob1 moduleMasses =
    let input = stringInputToInts moduleMasses
    in sum (map (\mass -> mass `div` 3 - 2) input)

stringInputToInts :: String -> [Int]
stringInputToInts bigstring =
    map read intstrings
    where intstrings = lines bigstring
