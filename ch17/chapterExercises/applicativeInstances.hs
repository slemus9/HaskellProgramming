-- ex.1 Pair
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (Pair f g) <*> (Pair a b) = Pair (f a) (g b)

-- ex.2 Two
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure b = Two (mempty) b
  (Two a f) <*> (Two a' b) = Two (a `mappend` a') (f b)

-- ex.3 Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three (mempty) (mempty) c
  (Three a b f) <*> (Three a' b' c) =
    Three (a `mappend` a') (b `mappend` b') (f c)

-- ex4. Three'
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure b = Three' (mempty) b b
  (Three' a f f') <*> (Three' a' b b') =
    Three' (a `mappend` a') (f b) (f' b')

-- ex.5 Four
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c)
          => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  (Four a b c f) <*> (Four a' b' c' d) =
    Four (a `mappend` a') (b `mappend` b') (c `mappend` c') (f d)

-- ex.6 Four'
data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a) => Applicative (Four' a) where
  pure b = Four' mempty mempty mempty b
  (Four' a b c f) <*> (Four' a' b' c' d) =
    Four' (a `mappend` a') (b `mappend` b') (c `mappend` c') (f d)
