myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail (x:[]) = Nothing
myTail (_:xs) = Just xs
