myAbs :: Integer -> Integer
myAbs x
 | x < 0 = (-x)
 | otherwise = x

bloodNa :: Integer -> String
bloodNa x
 | x < 135 = "too low"
 | x > 145 = "too high"
 | otherwise = "just right"

-- Checks if the triangle formed by the three parameters is a right triangle
isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
 | a^2 + b^2 == c^2 = "Right On"
 | otherwise = "not right"

 -- Convert dog's age to human years
dogYrs :: Integer -> Integer
dogYrs x
 | x <= 0 = 0
 | x <= 1 = x * 15
 | x <= 2 = x * 12
 | x <= 4 = x * 8
 | otherwise = x * 6

-- Returns the grade of an exam of 100 questions
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
 | y >= 0.9 = 'A'
 | y >= 0.8 = 'B'
 | y >= 0.7 = 'C'
 | y >= 0.59 = 'D'
 | y < 0.59 = 'F'
 where
   y = x/100

-- Checks for palindromes
pal xs
 | xs == reverse xs = True
 | otherwise = False

-- Compares numbers
numbers x
 | x < 0 = -1
 | x == 0 = 0
 | x > 0 = 1
