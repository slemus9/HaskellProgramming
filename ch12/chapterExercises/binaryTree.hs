data BinaryTree a =
  Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Show, Ord)

unfold :: (a -> Maybe (a, b ,a)) -> a -> BinaryTree b
unfold f a =
  case f a of
    Just (x, y, x') -> Node (unfold f x) y (unfold f x')
    Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (build) 0 where
  build x
   | x < ne = Just (x + 1, x, x + 1)
   | otherwise = Nothing
