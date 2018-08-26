-- Insert an element at a given position into a list
t15 x list n = take (n-1) list ++ [x] ++ drop (n-1) list

main = do
    putStrLn "Insert list"
    input1 <- getLine
    putStrLn "Insert I"
    input2 <-getLine
    putStrLn "Insert K"
    input3 <-getLine
    let x = (read input1 :: [Int])
    let i = (read input2 :: Int)
    let k = (read input3 :: Int)
    print(t15 i x k)