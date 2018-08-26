-- Create a list containing all integers within a given range
t16 a b = [a..b]


main = do
    putStrLn "Insert first number"
    input1 <- getLine
    putStrLn "Insert second number"
    input2 <-getLine
    let n = (read input1 :: Int)
    let k = (read input2 :: Int)
    print(t16 n k)