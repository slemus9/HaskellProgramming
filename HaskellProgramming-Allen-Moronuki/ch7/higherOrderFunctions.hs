myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f = \ x y -> f y x

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

-- The same as:
returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' = returnLast

returnAfterApply :: (a -> b) -> a -> c -> b
returnAfterApply f = \ x y = f x
