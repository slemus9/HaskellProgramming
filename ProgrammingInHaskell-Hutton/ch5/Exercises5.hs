import Notes5
import Data.Char

{- 1. Using a list comprehension, give an expression that calculates the sum
1²+2²+...+100² of the first one hundred integer squares.
-}
sumOfSquares :: Int -> Int
sumOfSquares n = sum [x^2 | x <- [1..n]]
-- sumOfSquares 100 = 1²+2²+...+100²

{- 2. In a similar way to the function length,
show how the library function replicate :: Int → a → [a]
hat produces a list of identical elements can be defined using a
list comprehension.
-}
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

{- 3. A triple (x, y, z) of positive integers can be termed pythagorean if x²+
y² = z². Using a list comprehension, define a function pyths :: Int →
[(Int, Int, Int )] that returns the list of all pythagorean triples whose
components are at most a given limit.
-}
pyths :: Int -> [(Int, Int, Int)]
pyths l = [(x,y,z) | x <- ns, y <- ns, z <- ns, x^2 + y^2 == z^2]
          where ns = [1..l]

{- 4. A positive integer is perfect if it equals the sum of its factors,
excluding the number itself. Using a list comprehension and the
function factors, define a function perfects :: Int → [Int ]
that returns the list of all perfect numbers up to a given limit.
-}
factors' :: Int -> [Int]
factors' n = [x | x <- [1..n-1], n `mod` x == 0]

perfects :: Int -> [Int]
perfects l = [x | x <- ns, (sum $ factors' x) == x]
              where
                ns = [1..l]

{- 5. Show how the single comprehension [(x , y) | x ← [1, 2, 3], y ← [4, 5, 6]]
with two generators can be re-expressed using two comprehensions with
single generators.
-}
regen = concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]

-- 6. Redefine the function positions using the function find.

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x (zip xs [0 .. n-1])
                  where n = length xs

{- 7. The scalar product of two lists of integers xs and ys of length n is given
by the sum of the products of corresponding integers:
  \sum_{i=0}^{n-1} (xs_i * ys_i)
-}
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x*y | (x,y) <- zip xs ys]

-- 8. Modify the Caesar cipher program to also handle upper-case letters.
let2int' :: Char -> Int
let2int' c
  | isLower c = ord c - ord 'a'
  | otherwise = ord c - ord 'A'

int2let' :: Int -> Bool -> Char
int2let' n isLow
  | isLow = chr (ord 'a' + n)
  | otherwise = chr (ord 'A' + n)

shift' :: Int -> Char -> Char
shift' n c
  | isLower c = int2let' ((let2int' c + n) `mod` 26) True
  | isUpper c = int2let' ((let2int' c + n) `mod` 26) False
  | otherwise = c

encode' :: Int -> String -> String
encode' n xs = [shift' n x | x <- xs ]
