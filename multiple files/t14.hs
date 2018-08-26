-- Remove the K'th element from a list
t14 list k = (list !! (k - 1), take (k - 1) list ++ drop k list)

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
    print(t14 x k)
