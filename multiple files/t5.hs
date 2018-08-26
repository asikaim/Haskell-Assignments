-- Reverse a list
t5 list = reverse list

split :: String -> [String]
split [] = [""]
split (c:cs) | c == ','  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs
    
main = do
    putStrLn "Insert list"
    param <- getLine
    let x = (split param)
    print(t5 x)