-- Determine the greatest common divisor of two positive integer numbers
t19 a b
  | b == 0     = abs a
  | otherwise  = t19 b (a `mod` b)


main = do
    putStrLn "Insert first number"
    input1 <- getLine
    putStrLn "Insert second number"
    input2 <-getLine
    let n = (read input1 :: Int)
    let k = (read input2 :: Int)
    print(t19 n k)