-- Drop every N'th element from a list
t10 :: [a] -> Int -> [a]
t10 xs n = helper xs n
  where helper [] _ = []
        helper (x:xs) 1 = helper xs n
        helper (x:xs) k = x : helper xs (k-1)

split :: String -> [String]
split [] = [""]
split (c:cs) | c == ','  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs


main = do
    putStrLn "Insert list"
    input1 <- getLine
    putStrLn "Insert N"
    input2 <-getLine
    let x = (split input1)
    let n = (read input2 :: Int)
    print(t10 x n)