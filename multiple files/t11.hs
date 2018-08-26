-- Split a list into two parts; the length of the first part is given
t11 list n = splitAt n list

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
    print(t11 x k)