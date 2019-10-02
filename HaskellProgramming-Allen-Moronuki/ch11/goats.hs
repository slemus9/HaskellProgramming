{-# LANGUAGE GeneralizedNewtypeDeriving #-}

class TooMany a where
  tooMany :: a -> Bool

newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

-- one key contrast between a newtype and a type alias is
--  that you can define typeclass instances for newtypes that differ from
--  the instances for their underlying type


instance TooMany Int where
  tooMany n = n > 42

{- We don´t have to define an instance of TooMany for Goats due to the prgama
instance TooMany Goats where
  tooMany (Goats n) = n > 42
- Instead we derive:
newtype Goats = Goats Int deriving (Eq, Show, TooMany)
-}
