-- Find the number of elements of a list
t4 list = length list

split :: String -> [String]
split [] = [""]
split (c:cs) | c == ','  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs


main = do
    putStrLn "Insert list"
    param <- getLine
    let x = (split param)
    print(t4 x)