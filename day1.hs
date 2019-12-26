import System.Environment

-- usage: day1 [input-file] [1 or 2]

main :: IO ()
main = do
    [fileName, probNum] <- getArgs
    fileContents <- readFile fileName
    let input = stringInputToInts fileContents
    case probNum of
        "1" -> print (prob1 input)
        "2" -> print (prob2 input)
        _ -> error "Invalid probnum"

prob1 :: [Int] -> Int
prob1 input =
    sum (map fuelForModule input)

prob2 :: [Int] -> Int
prob2 input = 
    sum (map (\mass -> fuelForModuleComplete mass 0) input)

fuelForModule :: Int -> Int
fuelForModule mass =
    if fuelMass >= 0 then fuelMass else 0
    where fuelMass = mass `div` 3 - 2

fuelForModuleComplete :: Int -> Int -> Int
fuelForModuleComplete 0 acc = acc
fuelForModuleComplete mass acc =
    fuelForModuleComplete fuel (acc + fuel)
    where fuel = fuelForModule mass

stringInputToInts :: String -> [Int]
stringInputToInts bigstring =
    map read intstrings
    where intstrings = lines bigstring
