import Data.Monoid

data Booly a = True' | False' deriving (Eq, Show)

-- Monoid instance for conjunction
instance Monoid (Booly a) where
  mappend False' _ = False'
  mappend _ False' = False'
  mappend True' True' = True'
