-- ex.1
tensDigit :: Integral a => a -> a
{-
tensDigit x = d where
  xLast = x `div` 10
  d = xLast `mod` 10
Rewrite using divMod: -}
divMod10 x = divMod x 10

tensDigit x = d where
  xLast = fst (divMod10 x)
  d = snd (divMod10 xLast)

-- Write a function to obtain the hundreds digit
hunsD x = d2 where
  xLast = x `div` 100
  d2 = xLast `mod` 10

-- ex.2 Implement foldBool using case matching and guards
foldBool :: a -> a -> Bool -> a
{- using Pattern matching:
foldBool x y True = x
foldBool x y False = y
-}
foldBool x y bool =
  case bool of
    True -> x
    False -> y

foldBoolGuards x y bool
  | bool = x
  | otherwise = y

-- ex.3 Implement the following function:
g :: (a -> b) -> (a, c) -> (b, c)

g aToB (a, c) = (aToB a, c)

-- ex.4 5 and 6 solved in the file arith4.hs
