x = (+)
f xs = w `x` 1 where
  w = length xs

-- Correction of \X = x
identity = \x -> x

--Correction of \x : xs -> x
getHead = \x -> head x

--Correction of f(a b) = A
f (a,b) = a
