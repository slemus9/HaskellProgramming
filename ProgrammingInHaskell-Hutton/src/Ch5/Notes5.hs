module Ch5.Notes5(
    lowers,
    count,
    positions,
    factors,
    find
) where

import Data.Char

-- ** List Comprehensions

-- * Generators

{- Examples:
To generate the set: {x² | x \in {1..5}} we use:
  [x^2 | x <- [1..5]] (| : "such that"; <- : "drawn from")

A expression such as x <- [1..5] is called a Generator.
A List Comprehension can have more than one Generator. The
following example generates a list of all possible pairings from
the respective lists:
  [(x, y) | x <- [1, 2, 3], y <- [4, 5]]

The following List Comprehension outputs the list of all possible
ordered pairings of the elements in [1..3]:
  [(x,y) | x <- [1..3], y <- [x..3]]

-}

-- Concatenates a list of lists
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

-- * Guards

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

{- Note that the function 'prime' does not require to produce
all of its factors, because under lazy evaluation, the result False
is returned as soon as any factor other than one or the number itself
is produced.
-}
prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k' ]

-- * The Zip Function
{- The library function zip produces a new list by pairing successive
elements from two existing lists until either or both are exhausted.
-}

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..n], x == x']
                  where n = length xs - 1

-- * String Comprehensions
{- String is just an abbreviation dor [Char].
Since strings are just special kinds of lists, any polymorphic function
on lists can also be used with strings. For example:
  zip "abc" [1,2,3,4]
-}

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']
