double :: Int -> Int
double x = x + x

quadruple :: Int -> Int
quadruple x = double (double x)

-- Factorial of a positive integer
factorial :: Int -> Int
factorial n = product [1..n]

-- Average of a list of integers
average :: [Int] -> Int
average ns = sum ns `div` length ns

-- Exercises:
{-
1. Parenthesise the arithmetic expressions:
  (2^3)*4
  (2*3) + (4*5)
  2 + 3*(4^5)
-}

-- 2. Correct the expression:
n = a `div` length xs
      where
        a = 10
        xs = [1, 2, 3, 4, 5]

-- 4. Define 'last' function which selects the last element in terms of the introduced functions of the chapter
last' :: [a] -> a
last' xs = xs !! (length xs - 1)

-- 5. Show how the function 'init' removes the last element from a non-empty list
init' :: [a] -> [a]
init' xs = take (length xs - 1) xs