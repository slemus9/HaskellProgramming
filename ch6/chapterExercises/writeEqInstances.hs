-- ex.1
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn i1) (TisAn i2) = i1 == i2

-- ex.2
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x1 y1) (Two x2 y2) = (x1 == x2) && (y1 == y2)

-- ex.3
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt i1) (TisAnInt i2) = i1 == i2
  (==) (TisAString s1) (TisAString s2) = s1 == s2
  (==) _ _ = False

-- ex.4
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair x y) (Pair x' y') = x == x' && y == y'

-- ex.5
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple x y) (Tuple x' y') = (x == x') && (y == y')

-- ex.6
data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne x) (ThisOne x') = x == x'
  (==) (ThatOne x) (ThatOne x') = x == x'
  (==) _ _ = False

-- ex.7
data EitherOr a b = Hello a | GoodBye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello x) (Hello x') = x == x'
  (==) (GoodBye x) (GoodBye x') = x == x'
  (==) _ _ = False
