module CheckLaws
    ( monoidAssoc
    , monoidLeftIdentity
    , monoidRightIdentity
    ) where

import Control.Monad
import Test.QuickCheck
import Data.Monoid
import Data.Semigroup as Sem

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [(1, return Fools), (1, return Twoo)]

instance Sem.Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where
  mempty = Fools
  mappend = (<>)
