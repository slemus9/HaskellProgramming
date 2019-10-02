-- ex.1
functionH :: [a] -> a
functionH (x:_) = x

-- ex.2
functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

-- ex.3
functionS :: (a, b) -> b
functionS (x, y) = y
