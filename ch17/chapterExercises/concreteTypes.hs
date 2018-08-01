-- ex.1 []
p1 :: a -> [] a
p1 = pure

a1 :: [] (a -> b) -> [] a -> [] b
a1 = (<*>)

-- ex.2 IO
p2 :: a -> IO a
p2 = pure

a2 :: IO (a -> b) -> IO a -> IO b
a2 = (<*>)

-- ex.3 (,) a
p3 :: Monoid a => b -> (,) a b
p3 = pure

a3 :: Monoid a => (,) a (c -> b) -> (,) a c -> (,) a b
a3 = (<*>)

-- ex.4 (->) e
p4 :: a -> (->) e a
p4 = pure

a4 :: (->) e (a -> b) -> (->) e a -> (->) e b
a4 = (<*>)
