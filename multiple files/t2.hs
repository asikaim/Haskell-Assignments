-- Find the last but one element of a list
t2 (x : _ : []) = x  
t2 (_ : xs)     = t2 xs

split :: String -> [String]
split [] = [""]
split (c:cs) | c == ','  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs

main = do
    putStrLn "Insert list"
    param <- getLine
    let x = (split param)
    print(t2 x)