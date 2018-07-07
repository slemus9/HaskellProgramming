eft :: (Ord a, Enum a) => a -> a -> [a]
eft a a'
 | a > a' = []
 | otherwise = a : (eft (succ a) a')

-- Succ True throws exception
eftBool :: Bool -> Bool -> [Bool]
eftBool True True = [True]
eftBool False False = [False]
eftBool False True = [False, True]
eftBool True False = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd o o' = eft o o'

eftInt :: Int -> Int -> [Int]
eftInt i i' = eft i i'

eftChar :: Char -> Char -> [Char]
eftChar c c' = eft c c'
