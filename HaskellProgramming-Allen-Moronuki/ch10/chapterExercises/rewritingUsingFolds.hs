-- ex.1
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- ex.2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x y -> f x || y) False

-- ex.3
myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr (\x y -> x == e || y) False

myElemAlt :: Eq a => a -> [a] -> Bool
myElemAlt e = any (\x -> x == e)

-- ex.4
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- ex.5
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\ x y -> f x : y ) []

-- ex.6
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr check [] where
  check x y
    | f x = x : y
    | otherwise = y

-- ex.7
squish :: [[a]] -> [a]
squish = foldr (\ xs y -> xs ++ y) []

-- ex.8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\ x y -> f x ++ y) [] where

-- ex.9
squishAgain :: [[a]] -> [a]
squishAgain = squishMap (id)

-- ex.10
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x : xs) = foldl comp x xs where
  comp x y
   | f x y == GT || f x y == EQ = x
   | otherwise = y

-- ex.11
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x : xs) = foldl comp x xs where
  comp x y
   | f x y == LT || f x y == EQ = x
   | otherwise = y
