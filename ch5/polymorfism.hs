-- Is not possible for this function to return a value other than itself
point1 :: a -> a

-- For this function there are only two possible implementations
point2 :: a -> a -> a
point2 x y = x
-- point2 x y = y

-- For this function there is only one possible implementation
point3 :: a -> b -> b
point3 x y = y
