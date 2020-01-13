module Ch11.Exercises11(

) where

import Ch11.CountdownProblem

{- 1. Redefine the combinatorial function choices using a list comprehension
rather than the library functions concat and map.
    choices :: [a] -> [[a]]
    choices xs = (concat . (map perms)) (subs xs)
-}
choices :: [a] -> [[a]]
choices xs = [c | sub <- subs xs, c   <- perms sub]

{- 2. Define a recursive function isChoice :: Eq a ⇒ [a ] → [a ] → Bool that
decides if one list is chosen from another, without using the combinato-
rial functions perms and subs.
-}
remFirstOcc :: Eq a => a -> [a] -> [a]
remFirstOcc x [] = []
remFirstOcc x (y : ys)
    | x == y = ys
    | otherwise = y : remFirstOcc x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice xs ys = go xs ys (length ys) where
    go :: Eq a => [a] -> [a] -> Int -> Bool
    go (x : xs) [] _ = False
    go [] _ _        = True
    go (x : xs) ys prevlen =
        let 
            ys' = remFirstOcc x ys
            newlen = length ys'
        in 
            (prevlen /= newlen) && go xs ys' newlen

isChoice' :: Eq a => [a] -> [a] -> Bool
isChoice' (x : xs) []       = False
isChoice' [] _              = True
isChoice' ls@(x : xs) (y : ys)
    | x == y = isChoice' xs ys
    | otherwise = isChoice' ls ys

{- 3. What effect would generalising the function split to also return pairs
containing the empty list have on the behaviour of solutions?

    The function exprs wouldn't terminate since the original list wouldn't reduce
    its lenght. Consider the already established definition of exprs

    exprs [] = []
    exprs [n] = [Val n]
    exprs ns = [e | (ls, rs) <- split ns,
                    l <- exprs ls,
                    r <- exprs rs,
                    e <- combine l r] 

    if for example, ls = [] and rs = ns (as a result of split), in the next iteration
    of exprs rs, it's possible that ls and rs are still equal to [] and ns (or the other
    way around)
-}

{- 4. Using choices, exprs and eval , verify that there are 33, 665, 406 possible
expressions over the numbers 1, 3, 7, 10, 25, 50, and that only 4, 672, 540
of these expressions evaluate successfully.
-}
possExpressions :: [Int] -> [Expr]
possExpressions xs = [e | cs <- choices xs, e <- exprs cs]

numPossExpressions :: [Int] -> Int
numPossExpressions = length . possExpressions

numSuccEvaluated :: [Int] -> Int
numSuccEvaluated = length . (filter $ \e -> eval e /= []) . possExpressions

{- 5. Similarly, verify that the number of expressions that evaluate successfully
increases to 10, 839, 369 if the numeric domain is generalised to arbitrary
integers. 
-}
valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = True
valid' Mul x y = y /= 1 && x /= 1 && x <= y
valid' Div x y = y /= 0 && x `mod` y == 0

{- 6. Modify the final program to:
    (a) allow the use of exponentiation in expressions;
    (b) produce the nearest solutions if no exact solution is possible;
    (c) order the solutions using a suitable measure of simplicity.

    SOLVED in file CountdownProblem.hs
-}