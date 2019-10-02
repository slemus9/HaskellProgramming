-- Decide if a character is a digit:
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

-- Decide if an integer s even:
even' :: Integral a => a -> Bool
even' n = n `mod` 2 == 0

-- Split a list at the nth element
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

-- Reciprocation
recip' :: Fractional a => a -> a
recip' n = 1/n

-- Conditional Expressions:

abs' :: Int -> Int
abs' n = if n >= 0 then n else -n

signum' :: Int -> Int
signum' n = if n < 0 then -1 else
              if n == 0 then 0 else 1

-- Guarded Equations

abs'' :: Int -> Int
abs'' n | n >= 0 = n
        | otherwise = -n

signum'' :: Int -> Int
signum'' n
  | n < 0 = -1
  | n == 0 = 0
  | otherwise = 1

-- Pattern Matching

not' :: Bool -> Bool
not' False = True
not' True = False

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, y) = y

-- Decides if the list contains exactly three characters at the beginning with 'a'
test :: [Char] -> Bool
test ('a':_) = True
test _ = False

null' :: [a] -> Bool
null' [] = True
null' (_:_) = False

head' :: [a] -> a
head' (x:_) = x

tail' :: [a] -> [a]
tail' (_:xs) = xs