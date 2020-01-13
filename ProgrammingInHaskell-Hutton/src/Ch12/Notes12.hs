module Ch12.Notes12(

) where

-- ** Lazy Evaluation

-- * Termination
inf :: Int
inf = 1 + inf

{- Using the call-by-value evaluation, the next
expression results in non-termination:
    
    fst (0, inf)
=   {applying inf}
    fst (0, 1 + inf)
=   {applying inf}
    fst (0, 1 + (1 + inf))
=   {applying inf}
    fst (0, 1 + (1 + inf (1 + inf))) 
    .
    .
    .

However if the call-by-name evaluation is used, it
terminates in one step

    fst (0, inf)
=   {applying fst}
    0

PROPERTY: if there exists any evaluation sequence that
terminates for a given expression, then call-by-name evaluation will also ter-
minate for this expression, and produce the same final result.
-}

-- * Number of reductions
{- Call-by-name evaluation may require more steps than
call-by-value evaluation, in particular when an argument is used more than
once in the body of a function.

PROPERTY: arguments are evaluated precisely once using call-by-value evaluation, but may
be evaluated many times using call-by-name.

Problem can be solved using pointers to repeated expressions (keep a copy
of the argument).

The use of call-by-name evaluation in conjunction with sharing is called
LAZY EVALUATION. This is the strategy of evaluation used by Haskell.

Lazy evaluation has the property that it ensures that evaluation terminates as often as possible.
Moreover, using sharing ensures that lazy evaluation never requires more steps than call-by-value 
evaluation.
-}

-- * Infinite Structures
ones :: [Int]
ones = 1 : ones

{-
PROPERTY: using lazy evaluation, expressions are only evaluated as
much as required by the context in which they are used.
-}

-- * Modular programming
primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- * Strict application
{-
An expression of the form f $! x is only a redex once evaluation of the argument x, 
using lazy evaluation as normal, has reached the point where it is known that the result 
is not an undefined value, at which point the expression can be reduced to the 
normal application f x.
-}
sumwith :: Int -> [Int] -> Int
sumwith v [] = v
sumwith v (x : xs) = sumwith (v + x) xs

{-
With Lazy evaluation, the entire summation is constructed before doing any additions,
which will cause the memory to be exhausted. Then, we would like to perform every addition
as soon as possible
-}
sumwith' v [] = v
sumwith' v (x : xs) = (sumwith' $! (v + x)) xs

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v [] = v
foldl' f v (x : xs) = ((foldl' f) $! (f v x)) xs

sumwith'' = foldl' (+)