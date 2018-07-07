-- ex.1
-- Each of the following functions are equivalent
mTh x y z = x * y * z
mTh1 x y = \z -> x * y * z
mTh2 x = \y -> \z -> x * y * z
mTh3 = \x -> \y -> \z -> x * y * z

-- ex.2 The type of mTh 3 is Num a => a -> a -> a

-- ex.3 Rewrite the functions with anonymous notation
--addOne x = x + 1
addOne = \x -> x + 1

-- a) Rewrite f:
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where
    -- f n = n + 1
    f = \n -> n + 1

-- b)
-- addFive x y = (if x > y then y else x) + 5
addFive = \x -> \y -> (if x > y then y else x) + 5

-- c) Rewrite so it doesn't use anonymous lambda syntax
-- mflip f = \x -> \y -> f y x
mflip f x y = f x y
