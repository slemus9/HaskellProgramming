{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
  tooMany :: a -> Bool

{-
newtype IntString = IntString (Int, String) deriving (Eq, Show)
newtype IntInt = IntInt (Int, Int) deriving (Eq, Show)

instance TooMany IntString where
  tooMany (IntString (i, _)) = i > 42

instance TooMany IntInt where
  tooMany (IntInt (i, i')) = (i + i') > 42
-}

instance TooMany (Int, String) where
  tooMany (n, _) = n > 42

instance TooMany (Int, Int) where
  tooMany (i, i') = (i + i') > 42

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany (x + y)
