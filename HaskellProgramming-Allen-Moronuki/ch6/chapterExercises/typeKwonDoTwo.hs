-- ex.1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = (aToB a) == b

-- ex.2
arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB int a = aToB a + fromIntegral int
