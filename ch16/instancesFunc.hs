import Test.QuickCheck
import Test.QuickCheck.Function
import FunctorLaws

-- ex.1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityCompose =
  Identity String ->
  Fun String String ->
  Fun String String ->
  Bool

-- ex.2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x x') = Pair (f x) (f x')

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

type PairCompose =
  Pair Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- ex.3
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoCompose =
  Two Int String ->
  Fun String String ->
  Fun String String ->
  Bool

-- ex.4
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
          Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeCompose =
  Three String String Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- ex.5
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) =>
          Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Three' a b b)

type ThreeCompose' =
  Three Bool Int Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- ex.6
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
          Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourCompose =
  Four String String String String ->
  Fun String String ->
  Fun String String ->
  Bool

-- ex.7
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) =>
          Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Four' a a a b)

type FourCompose' =
  Four' Char Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

main :: IO ()
main = do
  quickCheck (functorIdentity :: Identity String -> Bool)
  quickCheck (functorCompose' :: IdentityCompose)
  quickCheck (functorIdentity :: Pair String -> Bool)
  quickCheck (functorCompose' :: PairCompose)
  quickCheck (functorIdentity :: Two String Int -> Bool)
  quickCheck (functorCompose' :: TwoCompose)
  quickCheck (functorIdentity :: Three String Int Char -> Bool)
  quickCheck (functorCompose' :: ThreeCompose)
  quickCheck (functorIdentity :: Three' Int Char -> Bool)
  quickCheck (functorCompose' :: ThreeCompose')
  quickCheck (functorIdentity :: Four String String Int Char -> Bool)
  quickCheck (functorCompose' :: FourCompose)
  quickCheck (functorIdentity :: Four' Int Char -> Bool)
  quickCheck (functorCompose' :: FourCompose')
