-- Duplicate the elements of a list a given number of times
t9 list n = concatMap (replicate n) list

split :: String -> [String]
split [] = [""]
split (c:cs) | c == ','  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs

main = do
    putStrLn "Insert list"
    input1 <- getLine
    putStrLn "Insert K"
    input2 <-getLine
    let x = (split input1)
    let k = (read input2 :: Int)
    print(t9 x k)