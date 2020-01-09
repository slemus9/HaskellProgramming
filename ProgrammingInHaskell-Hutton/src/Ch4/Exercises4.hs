module Ch4.Exercises4(

) where

-- 1. Using library functions define a function halve :: [a] -> ([a],[a])
halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs)
            where half = (length xs) `div` 2

{- 2. Consider a function safetail :: [a ] -> [a ] that behaves as the library
function tail , except that safetail maps the empty list to itself, whereas
tail produces an error in this case
-}

-- 2.a. Using a conditional expression:
safetail :: [a] -> [a]
safetail xs = if null xs then [] else drop 1 xs

-- 2.b. Using guarded expressions:
safetail' :: [a] -> [a]
safetail' xs
  | null xs = []
  | otherwise = drop 1 xs

-- 2.c Using pattern matching:
safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (_:xs) = xs

{- 3. In a similar way to ∧, show how the logical disjunction operator ∨ can
be defined in four different ways using pattern matching.
-}
or1 :: Bool -> Bool -> Bool
or1 True True = True
or1 True False = True
or1 False True = True
or1 False False = False

or2 :: Bool -> Bool -> Bool
or2 False False = False
or2 _ _ = True

or3 :: Bool -> Bool -> Bool
or3 False b = b
or3 True _ = True

or4 :: Bool -> Bool -> Bool
or4 b c
  | b == c = b
  | otherwise = True

{- 4. Redefine the following version of the conjunction operator using condi-
tional expressions rather than pattern matching:
True ∧ True = True
_∧_ = False
-}
and1 :: Bool -> Bool -> Bool
and1 b c =
  if (b == True) then
    if (c == True) then True
    else False
  else False

{- 5. Do the same for the following version, and note the difference in the
number of conditional expressions required:
True ∧ b = b
False ∧ _ = False
-}
and2 :: Bool -> Bool -> Bool
and2 b c = if b then c else False

{- 6. Show how the curried function definition mult x y z = x ∗ y ∗ z can be
understood in terms of lambda expressions.
-}
mult :: Num a => a -> a -> a -> a
mult = \x -> (\y -> (\z -> x * y * z) )
