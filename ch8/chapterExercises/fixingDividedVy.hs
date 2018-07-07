-- Chapter 8 exercise

-- Handle negatives and division by cero

data DividedResult = Result Integer | DividedByZero deriving Show


divideByRec :: Integer -> Integer -> DividedResult
divideByRec num denom
  | num == 0 = Result 0
  | num < 0 && denom < 0 = Result (go (-num) (-denom) 0)
  | num < 0 = (Result . negate)  (go (-num) denom 0)
  | denom < 0 = (Result . negate) (go num (-denom) 0)
  | denom == 0 = DividedByZero
  | otherwise = Result (go num denom 0)
  where
    go n d count
     | n < d = count
     | otherwise = go (n - d) d (count + 1)
