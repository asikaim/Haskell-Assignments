import Data.List
import System.IO
{-
   Use ghci
   You can call functions writing t1, t2, t3 etc.
   Parameters can be given the sameway as in assignments
-}

-- Find the last element of a list
t1 :: [a] -> a
t1 (x:[]) = x
t1 (x:xs) = t1 xs

-- Find the last but one element of a list
t2 (x : _ : []) = x  
t2 (_ : xs)     = t2 xs

-- Find the K'th element of a list. The first element in the list is number 1
t3 :: [a] -> Int -> a
t3 list i    = list !! (i-1)

-- Find the number of elements of a list
t4 list = length list

-- Reverse a list
t5 list = reverse list

-- Find out whether a list is a palindrome
t6 list = if list == reverse list then True else False

-- Eliminate consecutive duplicates of list elements
t7 (x:ys@(y:_)) 
  | x == y = t7 ys 
  | otherwise = x : t7 ys 
t7 ys = ys

-- Duplicate the elements of a list
t8 [] = [] 
t8 (x:xs) = x:x:t8 xs

-- Duplicate the elements of a list a given number of times
t9 list n = concatMap (replicate n) list

-- Drop every N'th element from a list
t10 :: [a] -> Int -> [a]
t10 xs n = helper xs n
  where helper [] _ = []
        helper (x:xs) 1 = helper xs n
        helper (x:xs) k = x : helper xs (k-1)

-- Split a list into two parts; the length of the first part is given
t11 list n = splitAt n list

-- Extract a slice from a list
t12 :: [a] -> Int -> Int -> [a]
t12 list n n1  = fst (splitAt (n1-n+1)( snd ( splitAt (n - 1) list)))

-- Rotate a list N places to the left
t13 :: [a] -> Int -> [a]
t13 list n = take (length list) $ drop (length list + n) $ cycle list

-- Remove the K'th element from a list
t14 list k = (list !! (k - 1), take (k - 1) list ++ drop k list)

-- Insert an element at a given position into a list
t15 x list n = take (n-1) list ++ [x] ++ drop (n-1) list

-- Create a list containing all integers within a given range
t16 a b = [a..b]

-- Sorting a list of lists according to length of sublists 
t17 list = sortOn length list

-- Determine whether a given integer number is prime
t18 n | n < 4 = n > 1
t18 n = all ((/=0).mod n) $ 2:3:[x + i | x <- [6,12..s], i <- [-1,1]]
        where s = floor $ sqrt $ fromIntegral n

-- Determine the greatest common divisor of two positive integer numbers
t19 a b
  | b == 0     = abs a
  | otherwise  = t19 b (a `mod` b)

-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range
t20 a b 
  | even a = filter t18 [a+1,a+3..b]
  | True   = filter t18 [a,a+2..b]

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