data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving (Eq, Ord, Show)

-- Values on the left are less than values to the rigth by default. ex: Mon < Tue is True

-- The following definition gives preponderance to Fri
instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare - Fri = LT
  compare _ _ = EQ

-- Ord implies Eq. The following should compile
check' :: a -> a -> Bool
check' a a' = a == a'
