-- Determine whether a given integer number is prime
t18 n | n < 4 = n > 1
t18 n = all ((/=0).mod n) $ 2:3:[x + i | x <- [6,12..s], i <- [-1,1]]
        where s = floor $ sqrt $ fromIntegral n


main = do
    putStrLn "Insert number"
    input1 <- getLine
    let k = (read input1 :: Int)
    print(t18 k)