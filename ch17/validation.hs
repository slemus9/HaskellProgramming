data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure a = Success a
  (Failure e) <*> (Failure e') = Failure (e `mappend` e')
  (Success f) <*> (Success a)  = Success (f a)
  (Failure e) <*> _ = Failure e
  _ <*> (Failure e) = Failure e
