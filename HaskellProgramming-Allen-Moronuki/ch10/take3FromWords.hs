pab = ["Pizza", "Apple", "Banana"]
f a b = (take 3 (a :: String)) ++ (b :: String)
foldr f "" pab
