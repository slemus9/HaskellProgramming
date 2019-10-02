myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter cond (x:xs)
  | cond x = x : myFilter cond xs
  | otherwise = myFilter cond xs
