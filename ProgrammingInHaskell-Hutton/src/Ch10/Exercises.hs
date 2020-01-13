module Ch10.Exercises(

)where

import Ch10.Notes
import Ch10.TautologyChecker (Subst)

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
data Tree' = Leaf' Int | Node' Tree' Tree' deriving (Show)

numleafs :: Tree' -> Int
numleafs (Leaf' _) = 1
numleafs (Node' l r) = numleafs l + numleafs r

balanced :: Tree' -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) = abs (numleafs l - numleafs r ) <= 1 && balanced l && balanced r

{- 4. Define a function balance :: [Int ] → Tree that converts a non-empty list
of integers into a balanced tree.
-}

splitlist :: [a] -> ([a], [a])
splitlist xs = (take n xs, drop n xs) 
                where n = length xs `div` 2

balance :: [Int] -> Tree'
balance [x] = Leaf' x
balance xs = Node' (balance h1) (balance h2) 
             where
                (h1, h2) = splitlist xs

{- 5. Extend the tautology checker to support the use of logical disjunction
(∨) and equivalence (⇔) in propositions.
    SOLVED in file TaultologyChecker.hs
-}

{- 6. Using the function isTaut together with the parsing and interaction li-
braries from the previous two chapters, define an interactive tautology
checker that allows propositions to be entered from the keyboard in a
user-friendly syntax.
    SOLVED in file TautologyCheckerParser.hs
-}

{- 7. Extend the abstract machine to support the use of multiplication.
    SOLVED in file AbstractMachine.hs
-}

{- 8. Complete the following instance declarations:
    instance Monad Maybe where
        ...
    instance Monad [] where
-}

-- 8.1. For Maybe:
data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
    fmap _  Nothing' = Nothing'
    fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
    pure x = Just' x
    (Just' f) <*> (Just' x) = Just' (f x)
    _ <*> _ = Nothing' 

instance Monad Maybe' where
    return = Just'
    (Just' x) >>= f = f x
    Nothing'  >>= _ = Nothing' 

-- 8.2. For List:
joinLists :: List a -> List a -> List a
joinLists Nil ys = ys
joinLists (Cons x xs) ys = Cons x (joinLists xs ys)

concatList :: List (List a) -> List a
concatList = foldr joinLists Nil

instance Foldable List where
    foldr _ z Nil = z
    foldr f z (Cons x xs) = f x (foldr f z xs)

instance Functor List where
    fmap _ Nil          = Nil
    fmap f (Cons x xs)  = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    (Cons f fs) <*> xs = joinLists (fmap f xs)  (fs <*> xs)

instance Monad List where
    return x = Cons x Nil
--    (Cons x xs) >>= f = joinLists (f x) (xs >>= f)
    xs >>= f = concatList $ fmap f xs