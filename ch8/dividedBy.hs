type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

divideBy :: Numerator -> Denominator -> Quotient
divideBy = div

-- In the returned tuple (a0, a1), a0 is the Quotient and a1 is the remainder
-- go function: Haskell idiom to define a function via where-clause that can accept
--  more arguments than the top-level function.
divideByRec :: Integral a => a -> a -> (a, a)
divideByRec num denom = go num denom 0 where
  go n d count
   | n < d = (count, n)
   | otherwise = go (n - d) d (count + 1)
