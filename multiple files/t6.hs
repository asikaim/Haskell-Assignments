-- Find out whether a list is a palindrome
t6 list = if list == reverse list then True else False


main = do
    putStrLn "Insert list"
    param <- getLine
    print(t6 param)