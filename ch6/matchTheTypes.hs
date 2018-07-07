import Data.List (sort)
-- ex.1 Type cannot be replaced by a
i :: Num a => a
i = 1

-- ex.2 Type cannot be replaced by Num a => a
{-
f :: Float
f = 1.0
-}

-- ex.3 Latest type can be replaced by Fractional a => a
f :: Fractional a => a
f = 1.0

-- ex.4 Type can by replaced by RealFrac
--f1 :: float
f1 :: RealFrac a => a
f1 = 1.0

-- ex.5 Type can be replaced by Ord a => a -> a
--freud :: a -> a
freud :: Ord a => a -> a
freud x = x

-- ex.6 Type can be replaced by Int -> Int
--freud' :: a -> a
freud' :: Int -> Int
freud' x = x

-- ex.7 Type cannot be replaced by a -> a
myX = 1 :: Int
sigmund :: Int -> Int
sigmund x = myX

-- ex.8 Type cannot be replaced by Num a => a -> a
sigmund' :: Int -> Int
sigmund' x = myX

-- ex.9 Type can be replaced by [Int] -> Int
--jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

-- ex.10 Type can be replaced by Ord a => [a] -> a
--young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

-- ex.11 Type cannot be replaced by Ord a => [a] -> a
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs =  head (mySort xs)
