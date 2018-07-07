myHead :: [a] -> Maybe a

myHead [] = Nothing
myHead (x: _) = x
