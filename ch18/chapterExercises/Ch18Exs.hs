module Ch18Exs where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Write Monad instances for the following types

-- ex.1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

-- Instances for Testing

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

-- ex.2
data PhhhbbtttEither b a =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' b) = Right' b
  fmap f (Left' a) = Left' (f a)

instance Applicative (PhhhbbtttEither b) where
  pure a = Left' a
  (Right' b) <*> _ = Right' b
  _ <*> (Right' b) = Right' b
  (Left' f) <*> (Left' a) = Left' (f a)

instance Monad (PhhhbbtttEither b) where
  return = pure
  (Right' b) >>= _ = Right' b
  (Left' a)  >>= f = f a

-- Instances for Testing

instance (Arbitrary a, Arbitrary b) =>
          Arbitrary (PhhhbbtttEither a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Left' b, Right' a]

instance (Eq a, Eq b) =>  EqProp (PhhhbbtttEither a b) where
  (=-=) = eq

-- ex.3
data Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor (Identity) where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

-- Instances for Testing

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance (Eq a) =>  EqProp (Identity a) where
  (=-=) = eq

-- ex.4
data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

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

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _   = Nil
  _   <*> Nil = Nil
  (Cons f fs) <*> xs =
    (fmap f xs) `append` (fs <*> xs)

instance Monad List where
  return = pure
  xs >>= f = concat' $ fmap f xs

-- Instances for Testing

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return Nil),
               (10, return (Cons x y))]

instance (Eq a) =>  EqProp (List a) where
  (=-=) = eq

-- Write the following functions using the methods provided
-- by Monad and Functor

-- ex.1
j :: Monad m => m (m a) -> m a
j m = m >>= id

-- ex.2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f = fmap f
-- l1 f m = m >>= (\x -> return f x)

-- ex.3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f m1 m2 =
  m1 >>= (\ x
     -> m2 >>= (\ y
           -> return $ f x y))

-- ex.4
a :: Monad m => m a -> m (a -> b) -> m b
a m fs =
  fs >>= (\ f
     -> m >>= (\ x
          -> return $ f x))

-- ex.5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x : xs) f =
  f x >>= (\ y
      -> meh xs f >>= (\ ys
             -> return $ y : ys ))

-- ex.6
flipType :: (Monad m) => [m a] -> m [a]
flipType ms = meh ms id

type StringTuple = (String, String, String)

main :: IO ()
main = do
  putStrLn "Testing Nope"
  quickBatch (functor $ (NopeDotJpg :: Nope StringTuple))
  quickBatch (applicative $ (NopeDotJpg :: Nope StringTuple))
  quickBatch (monad $ (NopeDotJpg :: Nope StringTuple))
  putStrLn ""

  putStrLn "Testing PhhhbbtttEither"
  quickBatch (functor $ (Right' ("a", "a", "a") :: PhhhbbtttEither StringTuple StringTuple))
  quickBatch (applicative $ (Right' ("a", "a", "a") :: PhhhbbtttEither StringTuple StringTuple))
  quickBatch (monad $ (Right' ("a", "a", "a") :: PhhhbbtttEither StringTuple StringTuple))
  putStrLn ""

  putStrLn "Testing Identity"
  quickBatch (functor $ (Identity ("a", "a", "a") :: Identity StringTuple))
  quickBatch (applicative $ (Identity ("a", "a", "a") :: Identity StringTuple))
  quickBatch (monad $ (Identity ("a", "a", "a") :: Identity StringTuple))
  putStrLn ""

  putStrLn "Testing List"
  quickBatch (functor $ (Nil :: List StringTuple))
  quickBatch (applicative $ (Nil :: List StringTuple))
  quickBatch (monad $ (Nil :: List StringTuple))
  putStrLn ""
