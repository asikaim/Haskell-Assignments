-- Find the K'th element of a list. The first element in the list is number 1
t3 :: [a] -> Int -> a
t3 list i    = list !! (i-1)


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
    print(t3 x k)