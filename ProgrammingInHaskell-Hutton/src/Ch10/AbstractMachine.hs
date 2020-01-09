module Ch10.AbstractMachine(

)where

-- * Abstract machine
data Expr = Val Int | Add Expr Expr
data Op   = EVAL Expr | ADD Int

type Cont = [Op]

value :: Expr -> Int
-- value (Val n) = n
-- value (Add x y) = value x + value y
value e = eval e []

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD  n : c) m = exec c (n + m)
