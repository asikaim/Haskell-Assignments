-- Eliminate consecutive duplicates of list elements
t7 (x:ys@(y:_)) 
  | x == y = t7 ys 
  | otherwise = x : t7 ys 
t7 ys = ys

split :: String -> [String]
split [] = [""]
split (c:cs) | c == ','  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs

main = do
    putStrLn "Insert list"
    param <- getLine
    let x = (split param)
    print(t7 x)