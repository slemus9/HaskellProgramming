module Ch13.Notes (
    Expr (Val, Add),
    Op (PUSH, ADD),
    Code
) where

-- * Reasoning about programs

-- * Making append vanish
{- 
The function reverse has a complexity of O(n^2)

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

We would like to define a new reverse' function such that:
    reverse' xs ys = reverse xs ++ ys
=>  reverse xs = reverse' xs []

We perform induction on xs to verify the equation:

- Base Case for xs = []:
    reverse' [] ys
=   {applying reverse'}
    reverse [] ++ ys
=   {applying reverse}
    [] ++ ys
=   {applying ++}
    ys

- Inductive case for (x : xs): 

(I.H: reverse' xs ys = reverse xs ++ ys)

    reverse' (x : xs) ys
=   {applying reverse'}
    reverse (x : xs) ++ ys
=   {applying reverse}
    (reverse xs ++ [x]) ++ ys
=   {associativity of ++}
    reverse xs ++ ([x] ++ ys)
=   (induction hypothesis)
    reverse' xs (x : ys)

Then the definition for reverse' should be:
-}
reverse' :: [a] -> [a] -> [a]
reverse' [] ys = ys
reverse' (x : xs) ys = reverse' xs (x : ys)

newReverse :: [a] -> [a]
newReverse xs = reverse' xs []

-- The same behaviour of accumulation can be obtained with foldl (:) []

-- Induction on Tree-like structures
data Tree = Leaf Int | Node Tree Tree



{-
flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l r) = flatten l ++ flatten r

We would like to improve the efficientcy of flatten via a new function
flatten' which (similar as with the exercise with reverse), combines the
behaviours of flatten and ++
    flatten' t ns = flatten t ++ ns
=>  flatten = flatten' t []

- Base case for t = Leaf n:
    flatten' Leaf n ns
=   {applying flatten'}
    flatten (Leaf n) ++ ns
=   {applying flatten}
    [n] ++ ns
=   {applying ++}
    n : ns

- Inductive case for (Node l r)
    flatten' (Node l r) ns
=   {applying flatten'}
    flatten (Node l r) ++ ns
=   {applying flatten}
    flatten l ++ (flatten r ++ ns)
=   {Inductive Hypothesis for l}
    flatten' l (flatten r ++ ns)
=   {Inductive Hypothesis for r}
    flatten' l (flatten' r ns)

Hence:
-}

flatten' :: Tree -> [Int] -> [Int]
flatten' (Leaf n) ns = n : ns
flatten' (Node l r) ns = flatten' l (flatten' r ns)

flatten :: Tree -> [Int]
flatten t = flatten' t []

-- * Compiler correctness
data Expr = Val Int | Add Expr Expr
data Op   = PUSH Int | ADD

type Stack = [Int]
type Code  = [Op]

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH n : c) s = exec c (n : s)
exec (ADD : c) (m : n : s) = exec c (n + m : s)

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

{-
The correctness of the compiler can be expressed by the following
equation:
    exec (comp e) [] = [eval e]

This can be generalized to any initial stack s:
    exec (comp e) s = eval e : s

We can prove the correctness of the compiler by using induction over
the type Expr:

- Base case for e = Val n
    exec (comp (Val n)) s
=   {applying comp}
    exec [PUSH n] s
=   {applying exec}
    exec [] (n : s)
=   {applying exec}
    n : s
=   {unapplying eval}
    eval (Val n) : s

- Inductive case for (Add x y):
    exec (comp (Add x y)) s
=   {applying comp}
    exec (comp x ++ (comp y ++ [ADD])) s
=   {distributivity of exec}
    exec (comp y ++ [ADD]) (exec (comp x) s)
=   {induction hypothesis for x}
    exec (comp y ++ [ADD]) (eval x : s)
=   {distributivity of exec}
    exec [ADD] (exec (comp y) (eval x : s))
=   {induction hypothesis for y}
    exec [ADD] (eval y : (eval x : s))
=   {applying exec}
    exec [] (eval y + eval x : s)
=   {applying exec}
    (eval x + eval y) : s
=   {unapplying eval}
    eval (Add x y ) : s

Now we nees to prove the distributivity of exec that states:
    exec (c ++ d) s = exec d (exec c s)

- Base case for c = []_
    exec ([] ++ d) s
=   {applying ++}
    exec d s
=   {unapplying exec}
    exec d (exec [] s)

- Inductive case for PUSH n : c:
    exec ((PUSH n : c) ++ d) s
=   {applying ++}
    exec (PUSH n : (c ++ d)) s
=   {applying exec}
    exec (c ++ d) (n : s)
=   {Induction hypothesis}
    exec d (exec c (n : s))
=   {unapplying exec}
    exec d (exec (PUSH n : c) s)

- Inductive case for ADD : c 
    exec ((ADD : c) ++ d) s
=   {applying ++}
    exec (ADD : (c ++ d)) s
=   {deconstruct s}
    exec (ADD : (c ++ d)) (m : n : s')
=   {applying exec}
    exec (c ++ d) (n + m : s')
=   {Induction hypothesis}
    exec d (exec c (n + m : s'))
=   {unapplying exec}
    exec d (exec (ADD : c) s)

If the stack does not have the form s = m : n : s', it creates
a stack underflow error, however this can be avoided  eliminating
the use of append:
    comp' e c = comp e ++ c
=>  comp e = comp' e []

We obtain a similar expression as we did on the Tree data
-}
comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))

{- Now the correctness of the compiler can be proved by using induction
to verify that:
    exec (comp' e c) s = exec c (eval e : s)
-}