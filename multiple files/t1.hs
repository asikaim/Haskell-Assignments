-- Find the last element of a list
t1 :: [a] -> a
t1 (x:[]) = x
t1 (x:xs) = t1 xs


split :: String -> [String]
split [] = [""]
split (c:cs) | c == ','  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs

main = do
    putStrLn "Insert list"
    param <- getLine
    let x = (split param)
    print(t1 x)