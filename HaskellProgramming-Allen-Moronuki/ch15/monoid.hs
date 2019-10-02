class Monoid' m where
  mempty' :: m
  mappend' :: m -> m -> m
  mconcat' :: [m] -> m
  mconcat' = foldr mappend' mempty'

data Server = Server String
newtype Server' = Server' String

-- newtype is like a single-member C union that avoids creating
-- an extra pointer.
