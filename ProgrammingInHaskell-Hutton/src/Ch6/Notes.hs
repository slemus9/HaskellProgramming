module Ch6.Notes(

) where

-- ** Recursive Functions

-- * Basics:
length' :: [a]  -> Int
length' [] = 0
length' (_ : xs) = 1 + length' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

init' :: [a] -> [a]
init' [_] = []
init' (x : xs) = x : init' xs

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x : xs) ys = x : (xs `append` ys)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys)
  | x <= y = x : y : ys
  | otherwise = y : (x `insert` ys)

-- Insertion sort
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs) = insert x (isort xs)

-- * Multiple Arguments
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x,y) : zip' xs ys

-- Can be generalised to: drop' :: Integral b => b -> [a] -> [a]
-- Missing: case of negative integers
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' i [] = []
drop' i (_ : xs) = drop' (i - 1) xs

-- * Multiple Recursion
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
                  where
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]

-- * Mutual Recursion
-- Even and odd for non-negative integers
even' :: Int -> Bool
even' 0 = True
even' n = odd' (n - 1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n - 1)

evens :: [a] -> [a]
evens [] = []
evens (x : xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_ : xs) = evens xs
