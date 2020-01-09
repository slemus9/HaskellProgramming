module Ch1.Notes1(

) where

-- ** Introduction

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product(xs)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort(x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                  smaller = [a | a <- xs, a <= x]
                  larger = [b | b <- xs, b > x]
