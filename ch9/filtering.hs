-- ex.1
multiplesOf3 :: Integral a => [a] -> [a]
multiplesOf3 xs = filter (\x -> rem x 3 == 0) xs

-- ex.2
howManyMultiplesOf3 :: [Integer] -> Int
howManyMultiplesOf3 = length . multiplesOf3

-- ex.3
removeArticles :: String -> [String]
removeArticles xs
 = filter (\x -> not (x == "the" || x == "a" || x == "an") ) (words xs)
