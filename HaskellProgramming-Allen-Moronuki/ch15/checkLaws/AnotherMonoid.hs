module AnotherMonoid where

import CheckLaws
import Test.QuickCheck
import Data.Monoid
import Data.Semigroup as Sem

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Sem.Semigroup (Optional a) where
  Nada <> o = o
  o <> Nada = o
  (Only x) <> (Only y) = Only (x <> y)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend = (<>)

newtype First' a =
  First' {getFirst' :: Optional a} deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [(2, return $ First' (Only a)), (1, return $ First' Nada)]

instance Sem.Semigroup (First' a) where
  First' Nada <> ft = ft 
  ft <> _ = ft

instance Monoid (First' a) where
  mempty = First' Nada
  mappend = (<>)

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
