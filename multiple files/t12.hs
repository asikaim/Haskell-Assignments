-- Extract a slice from a list
t12 :: [a] -> Int -> Int -> [a]
t12 list n n1  = fst (splitAt (n1-n+1)( snd ( splitAt (n - 1) list)))

split :: String -> [String]
split [] = [""]
split (c:cs) | c == ','  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs

main = do
    putStrLn "Insert list"
    input1 <- getLine
    putStrLn "Insert I"
    input2 <-getLine
    putStrLn "Insert K"
    input3 <-getLine
    let x = (split input1)
    let i = (read input2 :: Int)
    let k = (read input3 :: Int)
    print(t12 x i k)