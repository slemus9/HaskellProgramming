myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f z [] = z
myFoldr f z (x : xs) = f x (myFoldr f z xs)

myFoldrAlt :: (a -> b -> b) -> b -> [a] -> b
myFoldrAlt f z xs =
  case xs of
    [] -> z
    (x : xs) -> f x (myFoldrAlt f z xs)
