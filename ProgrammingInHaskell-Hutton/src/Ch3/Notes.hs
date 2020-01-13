module Ch3.Notes(

) where

-- ** Types and Classes

-- Exercises:

{-
1. What are the types of the following values?

['a','b','c'] :: [Char]
('a','b','c') :: (Char, Char, Char)
[(False,'0'),(True,'1')] :: [(Bool,Char)]
([False,True],['0','1']) :: ([Bool],[Char])
[tail,init,reverse] :: [[a] -> [a]]

-}

--2. What are the types of the following functions?
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

{-
4.Why is it not feasible in general for function types to be instances of the
Eq class? When is it feasible? Hint: two functions of the same type are
equal if they always return equal results for equal arguments.

Since two functions are equal only if for the same arguments they provide the same
results, it is not feasible (in general) for them to be instances of Eq, as there
exists Surjective functions that might map more than one result for the same arguments.

It is not feasable since we would have to check every result for every argument of the function

It would only be feasable if we know that the function's domain if not infinite and small (such as Bool)

(Additional source: Rice Theorem.)

-}
