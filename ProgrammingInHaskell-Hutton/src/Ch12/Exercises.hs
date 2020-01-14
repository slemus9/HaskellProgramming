module Ch12.Exercises(

) where

{- 1. Identify the redexes in the following expressions, and determine whether
each redex is innermost, outermost, neither, or both:

1.1. 1 + (2 * 3)
    2 * 3: Innermost redex
    1 + (2 * 3): Outermost redex

1.2. (1 + 2) * (2 + 3)
    1 + 2: Innermost redex
    2 + 3: Redex
    (1 + 2) * (2 + 3): Outermost redex

1.3. fst (1 + 2, 2 + 3)
    1 + 2: Innermost redex
    2 + 3: Redex
    fst (1 + 2, 2 + 3): Outermost redex

1.4 (\x -> 1 + x) (2 * 3)
    2 * 3: Innermost redex
    (\x -> 1 + x) (2 * 3): Outermost
-}

{- 2. Show why outermost evaluation is preferable to innermost for the pur-
poses of evaluating the expression fst (1 + 2, 2 + 3).

- Outermost evaluation:
    fst (1 + 2, 2 + 3)
=   {applying fst}
    1 + 2
=   {applying +}
    3

- Innermost evaluation:
    fst (1 + 2, 2 + 3)
=   {applying +}
    fst (3, 2 + 3)
=   {applying +}
    fst (3, 5)
=   {applying fst}
    3

In this case the innermost has the unnecessary step of computing 2 + 3
-}

{- 3. Given the definition mult = λx → (λy → x ∗y), show how the evaluation
of mult 3 4 can be broken down into four separate steps.

    mult 3 4
=   (applying λx → (λy → x ∗y))
    (λy → 3 ∗ y) 4
=   (applying (λy → 3 ∗ y))
    3 * 4
=   (applying *)
    12
-}

{- 4. Using a list comprehension, define an expression fibs :: [Integer ] that
generates the infinite sequence of Fibonacci numbers. -}

fibs :: [Integer]
fibs = 0 : 1 : [f1 + f2 | (f1, f2) <- zip fibs (tail fibs) ]
-- fibs = zipWith (+) (1 : fibs) (0 : 1 : fibs)

{- 5. Using fibs, define a function fib :: Int → Integer that returns the nth Fi-
bonnaci number (counting from zero), and an expression that calculates
the first Fibonacci number greater than one thousand. -}

fib :: Int -> Integer
fib n = fibs !! n

fibGreaterThan :: Integer -> Integer
fibGreaterThan n = head $ dropWhile (<= n) fibs

-- The first fib greater than 1000 is 1597

{- 6. Define the functions repeat, take and replicate
for the following type of binary tree.
-}
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

repeat' :: a -> Tree a
repeat' x = t where t = Node t x t

take' :: Int -> Tree a -> Tree a
take' 0 _    = Leaf
take' n Leaf = Leaf
take' n (Node t1 x t2) = Node (take' (n - 1) t1) x (take' (n - 1) t2)

replicate' :: Int -> a -> Tree a
replicate' n = (take' n) . repeat'