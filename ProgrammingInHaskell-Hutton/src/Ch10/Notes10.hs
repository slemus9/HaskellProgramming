module Ch10.Notes10(
  Assoc,
  Nat(Zero, Succ),
  Tree(Leaf, Node),
  List(Nil, Cons),
  find,
  add
) where

-- ** Declaring Types and Classes

-- * Type declarations
{-
Type declarations establish synonyms for existing types.
Previous examples:
  type String = [Char]
  type Board = [Pos]
  type Pos = (Int, Int) 

Type declarations cannot be recursive, for example the following
declaration is NOT allowed:

  type Tree = (Int, [Tree])

Type declarations can also be parameterised:

  type Parser a = String -> [(a, String)]
  type IO a = World -> (a, World)
-}
type Pos = (Int, Int)
type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

-- * Data declarations
{-
Generate a completely new type.
  data Bool = False | True
-}
data Move = Lft | Rght | Up | Down

move :: Move -> Pos -> Pos
move Lft (x, y)  = (x - 1, y)
move Rght (x, y) = (x + 1, y)
move Up (x, y)    = (x, y - 1)
move Down (x, y)  = (x, y + 1)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m : ms) p = moves ms (move m p)

flip :: Move -> Move
flip Lft  = Rght
flip Rght = Lft
flip Up    = Down
flip Down  = Up

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r ^ 2
area (Rect x y) = x * y

{- Data declarations can also be parameterised, for example:
  data Maybe a = Nothing | Just a
-}
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just $ m `div` n

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just $ head xs

-- * Recursive types
-- Naturals
data Nat = Zero | Succ Nat deriving (Eq, Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ $ int2nat (n - 1)

add :: Nat -> Nat -> Nat
-- add m n = int2nat $ nat2int m + nat2int n
add Zero n = n
add (Succ m) n = Succ (add m n)

-- Lists
data List a = Nil | Cons a (List a) deriving Show

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs

-- Binary Trees
data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Eq a => a -> Tree a -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r) = (m == n) || (occurs m l) || (occurs m r)

flatten :: Tree a -> [a]
flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

occurs' :: (Eq a, Ord a) => a -> Tree a -> Bool
occurs' m (Leaf n) = m == n
occurs' m (Node l n r)
  | m == n    = True
  | m < n     = occurs m l
  | otherwise = occurs m r

{-
Some examples of various types of Trees:
  data Tree a = Leaf a | Node (Tree a) (Tree a)
  data Tree a = Leaf | Node (Tree a) a (Tree a)
  data Tree a b = Leaf a | Node (Tree a b) b (Tree a b)
  data Tree a = Node a [Tree a]
-}

-- * Class and instance declarations
{-
Examples:
  class Eq a where
    (==), (/=) :: a -> a -> Bool
    x /= y = not(x == y)
  
  instance Eq Bool where
    False == False = True
    True  == True  = True
    _     == _     = False

Only types declared using data declaration can be made into
instance classes.

Default definitions can be overridden in instance declarations.
-}

-- * Derived instances
{-
Makes a new type an instance of existing classes
Example:
  data Bool = False | True
              deriving (Eq, Ord, Show, Read)
-}

-- * Monadic types
{-
  class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
-}