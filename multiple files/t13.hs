-- Rotate a list N places to the left
t13 :: [a] -> Int -> [a]
t13 list n = take (length list) $ drop (length list + n) $ cycle list

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
    print(t13 x n)
