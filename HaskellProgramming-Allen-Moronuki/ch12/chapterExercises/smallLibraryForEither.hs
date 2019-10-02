lefts' :: [Either a b] -> [a]
lefts' = foldr (extractLeft) [] where
  extractLeft (Left l) lefts = l : lefts
  extractLeft (Right _) lefts = lefts

rights' :: [Either a b] -> [b]
rights' = foldr (extractRight) [] where
  extractRight (Right r) rights = r : rights
  extractRight (Left _) rights = rights

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right r) = Just (f r)
eitherMaybe' _ (Left _) = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left l) = f l
either' _ f (Right r) = f r

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e@(Right _) = Just (either' undefined f e)
eitherMaybe'' _ (Left _) = Nothing
