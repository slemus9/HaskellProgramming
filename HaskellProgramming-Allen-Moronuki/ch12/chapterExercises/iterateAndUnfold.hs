-- Can generate infinite list
myIterate :: (a -> a) -> a -> [a]
myIterate f z = z : myIterate f (f z)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =
  case f b of
    (Just (x, y)) -> x : myUnfoldr f y
    Nothing -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f a = myUnfoldr (\x -> Just (x, f x)) a 
