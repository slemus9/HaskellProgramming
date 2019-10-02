data List a =
  Nil | Cons a (List a)
  deriving (Eq, Show)

-- List Applicative Exercise

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ z Nil = z
fold f b (Cons x xs) = f x (fold f b xs)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _   = Nil
  _   <*> Nil = Nil
  (Cons f fs) <*> xs =
    (fmap f xs) `append` (fs <*> xs)

-- ZipList Applicative Exercise
take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

{-
instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l
-}

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

appendZ :: ZipList' a -> ZipList' a -> ZipList' a
appendZ (ZipList' xs) (ZipList' ys) = ZipList' (xs `append` ys)

instance Applicative ZipList' where
  pure a = ZipList' (pure a)
  (ZipList' l1) <*> (ZipList' l2) = ZipList' (buildList l1 l2) where
    buildList Nil _ = Nil
    buildList _ Nil = Nil
    buildList (Cons f fs) (Cons x xs) =
      Cons (f x) (buildList fs xs)
