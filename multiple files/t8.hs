-- Duplicate the elements of a list
t8 [] = [] 
t8 (x:xs) = x:x:t8 xs

split :: String -> [String]
split [] = [""]
split (c:cs) | c == ','  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs
    
main = do
    putStrLn "Insert list"
    param <- getLine
    let x = (split param)
    print(t8 x)