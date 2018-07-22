module SemigroupExercises where

import Data.Semigroup
import Test.QuickCheck

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- ex.1 Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- ex.2 Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc =
  Identity String -> Identity String -> Identity String -> Bool

-- ex.3 Two
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc =
  Two [Int] [Int] -> Two [Int] [Int] -> Two [Int] [Int] -> Bool

type TwoIns = Two String String

-- ex.4 Three
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c)
                      => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
                      => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssoc =
  Three [Int] String [Int] -> Three [Int] String [Int] -> Three [Int] String [Int]
      -> Bool

type ThreeIns = Three String String String

-- ex.5 Four
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
                                 => Semigroup (Four  a b c d) where
  (Four a b c d) <> (Four a' b' c' d') =
    Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
                                 => Arbitrary (Four  a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourAssoc =
  Four [Int] String [Int] String -> Four [Int] String [Int] String
   -> Four [Int] String [Int] String -> Bool

type FourIns = Four String String String String

-- ex.6 BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return $ BoolConj b

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- ex.7 BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  _ <> (BoolDisj True) = BoolDisj True
  _ <> _ = BoolDisj False

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = do
    b <- arbitrary
    elements [(BoolDisj b), (BoolDisj b)]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- ex.8 Or
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd a) <> _ = Snd a
  _ <> (Snd a) = Snd a
  _ <> o = o

instance (Arbitrary a, Arbitrary b)
                      => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(Fst a), (Snd b)]

type OrAssoc = Or Int Int -> Or Int Int -> Or Int Int -> Bool

-- ex.9 Combine
newtype Combine a b = Combine {unCombine :: (a -> b)}

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

-- ex.10 Comp

newtype Comp a = Comp {unComp :: (a -> a)}

instance Semigroup a => Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f <> g)

instance Monoid a => Monoid (Comp a) where
  mempty = Comp mempty
  mappend = (<>)

-- ex.11 Validation

data Validation a b  = Fail a | Succ b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Fail a) <> (Fail b) = Fail (a <> b)
  (Fail a) <> v = v
  (Succ a) <> _ = Succ a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(Fail a), (Succ b)]

type ValidationAssoc =
  (Validation String Int) -> (Validation String Int) -> (Validation String Int) -> Bool

-- ex.12 AccumulateRight

newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Succ a) <> AccumulateRight (Succ b) = AccumulateRight $ Succ (a <> b)
  AccumulateRight (Succ a) <> AccumulateRight (Fail b) = AccumulateRight (Fail b)
  AccumulateRight (Fail a) <> _ = AccumulateRight (Fail a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [AccumulateRight(Fail a), AccumulateRight(Succ b)]

type AccRightAssoc =
  (AccumulateRight String String) ->
   (AccumulateRight String String) ->
    (AccumulateRight String String) -> Bool
------------------------------------------------------------------------------
-- Mem exercise
-- Taken from https://github.com/andrewMacmurray/haskell-book-solutions/blob/master/src/ch15/Monoid.hs#L44-L60
newtype Mem s a = Mem {runMem :: s -> (a, s)}

instance Semigroup a => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) = Mem (\x ->
    let
      (a, b) = g x
      (c, d) = f b
    in
      (a <> c, d))

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem ( \ s -> (mempty, s))
  mappend = (<>)

f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: TwoIns -> Bool)
  quickCheck (monoidRightIdentity :: TwoIns -> Bool)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (monoidLeftIdentity :: ThreeIns -> Bool)
  quickCheck (monoidRightIdentity :: ThreeIns -> Bool)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (monoidLeftIdentity :: FourIns -> Bool)
  quickCheck (monoidRightIdentity :: FourIns -> Bool)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (semigroupAssoc :: AccRightAssoc)
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0
