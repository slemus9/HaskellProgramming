-- ex.1
i :: a -> a
i x = x

-- ex.2
c :: a -> b -> a
c x y = x

-- ex.3 (Alpha equivalence)
c'' :: b -> a -> b
c'' x y = x

-- ex.4
c' :: a -> b -> b
c' x y = y

-- ex.5
r :: [a] -> [a]
r arr = tail arr
-- r arr = reverse arr
-- r arr = init arr

-- ex.6
co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC (aToB a)

-- ex.7
a :: (a -> c) -> a -> a
a aToC a' = a'
-- a _ x = x

-- ex.8
a' :: (a -> b) -> a -> b
a' aToB a = aToB a 
