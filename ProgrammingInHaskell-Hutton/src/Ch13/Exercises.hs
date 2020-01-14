module Ch13.Exercises(

) where

import Ch13.Notes

{- 1. Give an example of a function from the standard library in appendix A
that is defined using overlapping patterns.

The functions using overlapping patterns rely on the order 
of definition in their pattern-matching. For example:
    foldl :: (a -> b -> a) -> a -> [b] -> a
    foldl _ v [] = v
    foldl f v (x : xs) = foldl f (f v x) xs
-}

{- 2. Show that add n (Succ m) = Succ (add n m), by induction on n.

- Base case for n = Zero:
    add Zero (Succ m)
=   {applying add}
    Succ m
=   {unapplying add}
    Succ (add Zero m)

- Inductive case for Succ n:
(I.H : add n (Succ m) = Succ (add n m))
    add (Succ n) (Succ m)
=   {applying add}
    Succ (add n (Succ m))
=   {I.H}
    Succ (Succ (add n m))
=   {unapplying add}
    Succ (add (Succ n) m)
-}

{- 3. Using this property, together with add n Zero = n, show that addition
is commutative, add n m = add m n, by induction on n.

- Base case for n = Zero
    add Zero m
=   {applying add}
    m
=   {add n Zero = n}
    add m Zero

- Inductive case for Succ n:
    add (Succ n) m
=   {applying add}
    Succ (add n m)
=   {I.H}
    Succ (add m n)
=   {add n (Succ m) = Succ (add n m)}
    add m (Succ n)
-}

{- 4. Complete the proof of the correctness of replicate by showing that it
produces a list with identical elements, all (== x ) (replicate n x), by
induction on n >= 0.
    replicate 0 _ = []
    retplicate (n + 1) x = x : replicate n x

    all' p [] = True
    all' p (x : xs) = p x && all p xs

We must show that the property is allways True

- Base case for n = 0
    all (== x) (replicate 0 x)
=   {applying replicate}
    all (== x) []
=   {applying all}
    True

- Inductive case for n + 1
(I.H.: all (== x) (replicate n x)) = Trues
    all (== x) (replicate (n + 1) x)
=   {applying replicate}
    all (== x) (x : replicate n x)
=   {applying all}
    (== x) x && all (== x) (replicate n x)
=   {I.H}
    (== x) x && True
=   {applying == and &&}
    True
-}

{- 5. Using the definition:
    [] ++ ys = ys
    (x : xs) ++ ys = x : (xs ++ ys)

verify the following properties
    1) xs ++ [] = xs
    2) xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

Verification for 1)
    - Base case for xs = []:
        [] ++ []
    =   {applying ++}
        [] (the second argument)

    -Inductive case for (x : xs):
    (I.H.: xs ++ [] = xs)
        (x : xs) ++ []
    =   {applying ++}
        x : (xs ++ [])
    =   {I.H}
        x : xs

Verification for 2)
    - Base case for xs = []:
        [] ++ (ys ++ zs)
    =   {applying ++}
        ys ++ zs
    =   {unapplying ++ for ys}
        ([] ++ ys) ++ zs
    
    - Inductive case for xs = (x : xs')
    (I.H.: xs' ++ (ys ++ zs) = (xs' ++ ys) ++ zs)
        (x : xs') ++ (ys ++ zs)
    =   {applying ++}
        x : (xs' ++ (ys ++ zs))
    =   {I.H}
        x : ((xs' ++ ys) ++ zs)
    =   {unapplying ++}
        (x : (xs' ++ ys)) ++ zs
    =   {unapplying ++}
        ((x : xs') ++ ys) ++ zs
-}

{- 6. In the case of this chapter, additional auxiliary results
led to the discovery of new rules that optimized the function. We could say
that various auxiliary results can end in more general expressions whereas a single
auxiliary result is only useful for the correctness of the specific function. -}

{- 7. Using the definitions:
    map f [] = []
    map f (x : xs) = f x : map f xs

    (f . g) x = f (g x)

Show that map f (map g xs) = map (f . g) xs by induction

- Base case for xs = []
    map f (map g [])
=   (applying map)
    map f []
=   (applying map)
    []
=   (unapplying map with f . g)
    map (f . g) []

- Inductive case for (x : xs)
(I.H.: map f (map g xs) = map (f . g) xs)
    map f (map g (x : xs))
=   (applying map)
    map f (g x : map g xs)
=   (applying map)
    f (g x) : map f (map g xs)
=   {I.H}
    f (g x) : map (f . g) xs
=   {definition f . g}
    (f . g) x : map (f . g) xs
=   {definition of map}
    map (f . g) (x : xs)
-}

{- 8. Using the following definitions:
    [] ++ ys = ys
    (x : xs) ++ ys = x : (xs ++ ys)

    take 0 _ = []
    take (n + 1) [] = []
    take (n + 1) (x : xs) = x : take n xs

    drop 0 xs = xs
    drop (n + 1) [] = []
    drop (n + 1) (_ : xs) = drop n xs

show that take n xs ++ drop n xs = xs, by simultaneous induction on the
integer n >= 0 and the list xs.

- Base case for n = 0
    take 0 xs ++ drop 0 xs
=   {applying take}
    [] ++ drop 0 xs
=   {applying drop}
    [] ++ xs
=   {applying ++}
    xs

- Base case for xs = [] and n > 0
    take (n + 1) [] ++ drop (n + 1) []
=   {applying take}
    [] ++ drop (n + 1) []
=   {applying drop}
    [] ++ []
=   {applying ++}
    []

- Inductive case for n + 1 and (x : xs)
(I.H.: take n xs ++ drop n xs = xs)
    take (n + 1) (x : xs) ++ drop (n + 1) (x : xs)
=   {applying take}
    (x : take n xs) ++ (drop n xs)
=   {applying ++}
    x : (take n xs ++ drop n xs)
=   {I.H}
    x : xs
-}

{- 9. Given the type declaration Tree, show that the number of leaves in a such a tree is always one greater
than the number of nodes, by induction on trees. 

We can define the following functions:
-}
data Tree = Leaf Int | Node Tree Tree

leafs :: Tree -> Int
leafs (Leaf _) = 1
leafs (Node t1 t2) = leafs t1 + leafs t2

nodes :: Tree -> Int
nodes (Leaf _) = 0
nodes (Node t1 t2) = 1 + nodes t1 + nodes t2

{-
We need to show that for any t :: Tree, leafs t  = 1 + nodes t.
To verify this we use induction:

- Base case for t = Leaf n
    leafs (Leaf n)
=   {applying leafs}
    1
=   {adding 0}
    1 + 0
=   {unapplying nodes}
    1 + nodes (Leaf n)

- Inductive case for t = Node t1 t2
(I.H.: leafs ti  = 1 + nodes ti)
    1 + nodes (Node t1 t2)
=   {applying nodes}
    1 + (1 + nodes t1 + nodes t2)
=   {associativity of +}
    (1 + nodes t1) + (1 + nodes t2)
=   {I.H over t1}
    leafs t1 + (1 + nodes t2)
=   {I.H over t2}
    leafs t1 + leafs t2
=   {definition of leafs}
    leafs (Node t1 t2)
-}

{- 10. Given the equation comp' e c = comp e ++ c, show how to construct the
recursive definition for comp' , by induction on e.

Definitions:
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

- Base case for e = Val n
    comp' (Val n) c
=   {applying comp'}
    comp (Val n) ++ c
=   {applying comp}
    [PUSH n] ++ c
=   {applying ++}
    PUSH n : (c ++ [])
=   {applying ++}
    PUSH n : c

- Inductive case for Add x y:
(I.H.: comp' e c = comp e ++ c)
    comp' (Add x y) c
=   {applying comp'}
    comp (Add x y) ++ c
=   {applying comp}
    comp x ++ comp y ++ [ADD] ++ c
=   {reducing [ADD] ++ c}
    comp x ++ comp y ++ (ADD : c)
=   {I.H over y}
    comp x ++ (comp' y (ADD : c))
=   {I.H- over x}
    comp' x (comp' y (ADD : c))

Hence the definition of comp' is:
-}
comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))