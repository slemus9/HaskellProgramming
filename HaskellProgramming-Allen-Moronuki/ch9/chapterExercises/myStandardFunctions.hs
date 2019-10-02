-- Chapter 9 exercise

-- book example
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs) = x && myAnd xs

-- ex.1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = x || myOr xs

-- ex.2
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x : xs)
 | f x = True
 | otherwise = False || myAny f xs

-- ex.3
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x : xs) = (a == x) || myElem a xs

myElemAlt :: Eq a => a -> [a] -> Bool
myElemAlt a xs = any (\x -> a == x) xs

-- ex.4
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = (myReverse xs) ++ (:[])x

-- ex.5
squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

-- ex.6
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x : xs) = f x ++ squishMap f xs

-- ex.7
squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap (id) xs

-- ex.8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : xs) = go f x xs where
  go _ x [] = x
  go f x (y : ys)
   | f x y == GT || f x y == EQ = go f x ys
   | otherwise = go f y ys

-- ex.8
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x : xs) = go f x xs where
  go _ x [] = x
  go f x (y : ys)
   | f x y == LT || f x y == EQ = go f x ys
   | otherwise = go f y ys

-- ex.9
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
