import Data.List
-- Sorting a list of lists according to length of sublists 
t17 list = sortOn length list

main = do
    putStrLn "Insert list"
    input1 <- getLine
    let x = (read input1 :: [[Int]])
    print(t17 x)