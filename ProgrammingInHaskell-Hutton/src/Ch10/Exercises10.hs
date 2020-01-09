module Ch10.Exercises10(

)where

import Ch10.Notes10

-- Exercises
{- 1. Using recursion and the function add , define a multiplication function
mult :: Nat → Nat → Nat for natural numbers. -}

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult m n = go m n n where
    go :: Nat -> Nat -> Nat -> Nat
    go (Succ Zero) _ acc = acc
    go (Succ m) n acc= go m n (add n acc) 

{- 2. Using data Ordering and the function compare redefine the funtion 
occurs:: Int -> Tree -> Bool
-}

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = EQ == compare x y
occurs x (Node l y r)
    | LT == compare x y = occurs x l
    | GT == compare x y = occurs x r
    | otherwise = True

{- 3. Define a function balanced :: Tree → Bool that decides if a tree is balanced or not,
taking into account the following definition:
-}
data Tree' = Leaf' Int | Node' Tree' Tree'

numleafs :: Tree' -> Int
numleafs (Leaf' _) = 1
numleafs (Node' l r) = numleafs l + numleafs r

balanced :: Tree' -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = abs (numleafs l - numleafs r ) <= 1 && balanced l && balanced r

{- 4. Define a function balance :: [Int ] → Tree that converts a non-empty list
of integers into a balanced tree.
-}
