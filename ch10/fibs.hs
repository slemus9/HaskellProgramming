myScanl :: (a -> b -> a) -> a -> [b] -> [a]
myScanl f q ls =
  q : ( case ls of
          []    -> []
          x:xs -> myScanl f (f q x) xs)

fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fibs20 = take 20 fibs
fibs100 = takeWhile (\x -> x < 100) fibs

factorials = scanl (*) 1 [1..]
