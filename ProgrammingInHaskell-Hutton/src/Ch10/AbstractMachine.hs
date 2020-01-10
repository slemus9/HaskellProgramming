module Ch10.AbstractMachine(

)where

-- * Abstract machine

-- Exercise 7
data Expr = Val Int 
            | Add  Expr Expr
            | Mult Expr Expr

data Op   = EVALA Expr | EVALM Expr
            | ADD  Int
            | MULT Int

type Cont = [Op]

value :: Expr -> Int
-- value (Val n) = n
-- value (Add x y) = value x + value y
value e = eval e []

eval :: Expr -> Cont -> Int
eval (Val  n)   c = exec c n
eval (Add  x y) c = eval x (EVALA y : c)
eval (Mult x y) c = eval x (EVALM y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVALA y : c) n = eval y (ADD n : c)
exec (ADD   n : c) m = exec c (n + m)
exec (EVALM y : c) n = eval y (MULT n : c)
exec (MULT  n : c) m = exec c (n * m)
