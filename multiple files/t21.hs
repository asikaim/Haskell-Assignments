-- Construct completely balanced binary trees 
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

t21 :: Int -> [Tree Char]
t21 0 = [Empty]
t21 1 = [leaf 'x']
t21 n = let (q, r) = (n - 1) `quotRem` 2
    in [Branch 'x' left right | i     <- [q .. q + r],
                                left  <- t21 i,
                                right <- t21 (n - i - 1)]


main = do
    putStrLn "Insert number"
    input1 <- getLine
    let n = (read input1 :: Int)
    print(t21 n)