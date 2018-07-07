-- Chapter 8 exercise

-- ex.2.
sumFromOne :: (Eq a, Num a) => a -> a

sumFromOne n = helper n - 1 where
  helper 0 = 1
  helper x = x + helper(x - 1)

sumAlt n = (n * (n + 1)) `div` 2

-- ex.3.
multBy :: Integral a => a -> a -> a

multBy a b
 | a == 1 = 1
 | b == 1 = a
 | otherwise = a + multBy a (b - 1)
