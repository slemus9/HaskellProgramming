data Maybe a = Nothing | Just a

-- Turn the partial function f into a total function:
{-
f :: Bool -> Int
f False = 0
-}

f :: Bool -> Maybe Int
f False = Just 0
f _ = Nothing
