module Ch11.CountdownProblem(

) where

-- ** The Countdown Problem

-- * Formalising the problem

data Op = Add | Sub | Mul | Div deriving Show
data Expr = Val Int | App Op Expr Expr deriving Show

-- Check if the operations are valid within the Natural Numbers
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

-- Return the list of values in an expression
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ e1 e2) = values e1 ++ values e2

-- Evaluate and return the value of the expression
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o e1 e2) = [apply o x y | x <- eval e1,
                                    y <- eval e2,
                                    valid o x y ]

-- Return all the subsequences of a list
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x :) yss where
                yss = subs xs

-- Return all the possible ways to insert an element into a list
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

-- Returns all permutations of a list, given by all possible reorderings
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = (concat . (map $ interleave x)) (perms xs)

-- Returns all possible ways of selecting zero or more elements in any order
choices :: [a] -> [[a]]
choices xs = (concat . (map perms)) (subs xs)

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- * Brute force solution

