module Ch7.Notes7(

) where

-- ** Higher-Order Functions

{- A function that takes a function as an argument
or returns a function as a result is called higher-order.
-}

-- * Processing lists

-- Map
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

{- The original definition for map using a
list comprehension is simpler, but the recursive definition is preferable
for reasoning purposes.
-}
map'' f [] = []
map'' f (x : xs) = f x : map'' f xs

-- Filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' _ [] = []
filter'' p (x : xs)
  | p x = x : filter'' p xs
  | otherwise = filter'' p xs

sumsqreven :: [Int] -> Int
sumsqreven xs = sum $ map (^2) (filter even xs)

-- * The foldr function
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x : xs) = f x (foldr' f v xs)

{- It's better to think of the behaviour of foldr as simply
replacing each cons operator in a list by the function f, and the
empty list at the end by the value v. For example:
  For foldr (+) 0:
    1:(2:(3:[])) -> 1+(2+(3 + 0))

  For foldr (\_ n -> 1 + n) 0
    1:(2:(3:[])) -> 1+(1+(1 + 0))
-}

-- Examples using foldr:

-- Length
length' :: [a] -> Int
length' = foldr (\_ n -> 1 + n) 0

-- Reverse
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

reverse' :: [a] -> [a]
reverse' = foldr snoc []

-- * The foldl function

{- foldr reflects the use of an operator that is assumed to associate
to the right. In general we can sumarize foldr as:
  foldr (op) v [x_0,x_1,...,x_{n-1}] = x_0 (op) (x_1 (op) (...(x_{n-1} (op) v)...))

Similarly we can define a recursive function using an operator that is
assumed to associate to the left.
  foldl (op) v [x_0,x_1,...,x_{n-1}] = (...(v (op) x_0) (op) x_1 ...) (op) x_{n-1}

-}
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ v [] = v
foldl' f v (x : xs) = foldl' f (f v x) xs

-- * The composition operator

{- The operator (.) returns the composition of two functions as a
single function:
  (.) :: (b -> c) -> (a -> b) -> (a -> c)
  f.x = \x -> f (g x)

Function composition is associative, that is:
  f.(g.h) = (f.g).h

The id function (id = \x -> x) is the identity for composition, that is:
  f.id = id.f = f

-}
sumsqreven' = sum.(map (^2)).(filter even)

-- Composition of a list of functions:
compose :: [a -> a] -> (a -> a)
compose = foldr (.) id
