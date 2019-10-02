module WordNumber where

-- Chapter 8 exercise
import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
 | n == 0 = "zero"
 | n == 1 = "one"
 | n == 2 = "two"
 | n == 3 = "three"
 | n == 4 = "four"
 | n == 5 = "five"
 | n == 6 = "six"
 | n == 7 = "seven"
 | n == 8 = "eight"
 | n == 9 = "nine"

calculateDigit :: Integral a => a -> a -> a
calculateDigit n mult10 = (n `div` mult10) `mod` 10

digits :: Int -> [Int]
digits n = go n [] 1 where
  go n list mult10
   | n == 0 = (:[]) 0
   | n `div` mult10 == 0 = list
   | otherwise = go n (((:[]) (calculateDigit n mult10)) ++ list) (mult10 * 10)

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" (map digitToWord (digits n))
