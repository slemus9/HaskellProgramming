-- ex.1
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

-- ex.2
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys

-- ex.3
myZipAlt :: [a] -> [b] -> [(a, b)]
myZipAlt xs ys = myZipWith (\x y -> (x, y)) xs ys
