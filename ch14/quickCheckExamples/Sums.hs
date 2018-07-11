module Sums where

import Test.QuickCheck

data Sum a b = First a | Second b deriving (Eq, Show)

-- The oneof function will create a Gen a from a list of Gen a by giving
-- each value an equal probability.
sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a, return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

{-
-- you can choose a different weighting of probabilities for sum types
instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary =
    frequency [(1, return Nothing), (3, liftM Just arbitrary)]
    -- It’s making an arbitrary Just value three times more likely than
    -- a Nothing value
-}

sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a), (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls
