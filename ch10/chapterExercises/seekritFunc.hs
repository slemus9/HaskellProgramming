-- Chapter 10 excercise
-- ex. 2
seekritfunc :: String -> Int
seekritfunc x =
  div (sum (map length (words x)))
      (length (words x))

seekritfuncFrac :: Fractional a => String -> a
seekritfuncFrac x =
  ((fromIntegral . sum . map length . words) x) /
  ((fromIntegral . length . words) x)
