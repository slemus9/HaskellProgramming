module Ch6.Exercises6(

) where

{- 1. Define the exponentiation operator ↑ for non-negative integers using the
same pattern of recursion as the multiplication operator ∗, and show
how 2^3 is evaluated using your definition.
-}
pow :: Real a => a -> Int -> a
pow x 0 = 1
pow 0 n = 0
pow x n
  | even n = pow (x*x) (n `div` 2)
  | otherwise = x * (pow (x*x) ((n-1) `div` 2))

{-
  pow 2 3
= {applying pow}
  2 * (pow 4 1)
= {applying pow}
  2 * 4 * pow (16 0)
= {applying pow}
  2 * 4 * 1
= {applying *}
  8
-}

{- 2. Using the definitions given in this chapter, show how length [1, 2, 3],
drop 3 [1, 2, 3, 4, 5], and init [1, 2, 3] are evaluated.

- Evaluation of length [1,2,3]
  length [1,2,3]
= {applying length}
  1 + length [2,3]
= {applying length}
  1 + 1 + length [3]
= {applying length}
  1 + 1 + 1 + length []
= {applying length}
  1 + 1 + 1 + 0
= {applying +}
  3

- Evaluation of drop 3 [1,2,3,4,5]
  drop 3 [1,2,3,4,5]
= {applying drop}
  drop 2 [2,3,4,5]
= {applying drop}
  drop 1 [3,4,5]
= {applying drop}
  drop 0 [4.5]
= {applying drop}
  [4,5]

- Evaluation of init [1,2,3]
  init [1,2,3]
= {applying init}
  1 : init [2,3]
= {applying init}
  1 : 2 : init[3]
= {applying init}
  1 : 2 : []
= {applying :}
  [1,2]
-}

{- 3. Without looking at the definitions from the standard prelude, define the
following library functions using recursion:
-}

-- 3.1. Decide if all logical values in a list are True:
and' :: [Bool] -> Bool
and' [] = True
and' (b : bs) = b && and' bs

-- 3.2. Concatenate a list of lists:
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs : xss) = xs ++ concat' xss

-- 3.3. Produce a list with n identical elements:
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate (n - 1) x

-- 3.4. Select the nth element of a list:
at :: [a] -> Int -> a
at xs n
  | null xs || n < 0 || n >= length xs = error "empty list or index out of bounds"
  | otherwise = go xs n
    where
      go (x : xs) 0 = x
      go (x : xs) n = go xs (n - 1)

-- 3.5. Decide if a value is an element of a list:
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x : xs) = (e == x) || elem' e xs

{- 4. Define a recursive function merge :: Ord a ⇒ [a ] → [a ] → [a ] that
merges two sorted lists to give a single sorted list. For example:
-}
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge lx@(x : xs) ly@(y : ys)
  | x <= y = x : (merge xs ly)
  | otherwise = y : (merge lx ys)

{- 5. Using merge, define a recursive function msort :: Ord a ⇒ [a ] → [a ]
that implements merge sort, in which the empty list and singleton lists
are already sorted, and any other list is sorted by merging together the
two lists that result from sorting the two halves of the list separately.
-}
halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs)
            where half = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
            where (left, right) = halve xs

{- 6.Using the five step process, define the library functions that calculate
the sum of a list of numbers, take a given number of elements from the
start of a list, and select the last element of a non-empty list.
-}

-- 6.1. Sum of a list of numbers:
{-
Step 1: Define the type
  sum : [Int] -> Int

Step 2: Enumerate the cases
  sum [] =
  sum (x : xs) =

Step 3: Define the simple cases
  sum [] = 0

Step 4: Define the other cases
  sum [] = 0
  sum (x : xs) = x + Sum xs

Step 5: Generalise and simplify
  sum :: Num a => [a] -> a
  sum [] = 0
  sum (x : xs) = x + sum xs
-}
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x : xs) = x + sum' xs

-- 6.2. Take a given number of elements from the start of a list
{-
Step 1: Define the type
  take :: Int -> [a] -> [a]

Step 2: Enumerate the cases
  take 0 xs =
  take _ [] =
  take n (x : xs) =

Step 3: Define the simple cases
  take 0 xs = xs
  take _ [] = []

Step 4: Define the other cases
  take 0 xs = xs
  take _ [] = []
  take n (x : xs) = take (n - 1) xs

Step 5: Generalise and simplify
  take :: Int -> [a] -> [a]
  take 0 xs = xs
  take _ [] = []
  take n (x : xs) = take (n - 1) xs
-}
take' :: Int -> [a] -> [a]
take' 0 xs = xs
take' _ [] = []
take' n (x : xs) = take' (n - 1) xs

-- 6.3. Select the last element of a non-empty list.
{-
Step 1: Define the type
  last :: [a] -> a

Step 2: Enumerate the cases
  last [] =
  last (x : xs) =

Step 3: Define the simple cases
  last [] = error "The list cannot be empty"

Step 4: Define the other cases
  last (x : xs)
    | null xs = x
    | otherwise = last xs

Step 5: Generalise and simplify
  last :: [a] -> a
  last [] = error "The list cannot be empty"
  last (x : xs)
    | null xs = x
    | otherwise = last xs
-}
last' :: [a] -> a
last' [] = error "The list cannot be empty"
last' (x : xs)
  | null xs = x
  | otherwise = last' xs
