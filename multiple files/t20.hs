-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range
t20 a b 
  | even a = filter t18 [a+1,a+3..b]
  | True   = filter t18 [a,a+2..b]
  

t18 n | n < 4 = n > 1
t18 n = all ((/=0).mod n) $ 2:3:[x + i | x <- [6,12..s], i <- [-1,1]]
        where s = floor $ sqrt $ fromIntegral n


main = do
    putStrLn "Insert first number"
    input1 <- getLine
    putStrLn "Insert second number"
    input2 <-getLine
    let n = (read input1 :: Int)
    let k = (read input2 :: Int)
    print(t20 n k)