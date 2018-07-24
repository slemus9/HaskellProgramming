{-# LANGUAGE FlexibleInstances #-}
-- Rearrange arguments so the Functor instance works
-- ex.1
data Sum a b =
    First b
  | Second a
  deriving (Eq, Show)

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

-- ex.2
data Company a b c =
    DeepBlue a b
  | Something c
  deriving (Eq, Show)

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- ex.3
data More a b =
    L b a b
  | R a b a
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write the corresponding Functor instances
-- ex.1
data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

-- ex.2
data K a b =
  K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

-- ex.3
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip $ K (f b)

-- ex.4
data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- ex.5
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance (Functor f) => Functor (LiftItOut f) where
  fmap fun (LiftItOut fa) = LiftItOut $ fmap fun fa

-- ex.6
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap fun (DaWrappa fa ga) = DaWrappa (fmap fun fa) (fmap fun ga)

-- ex.7
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance (Functor f1) => Functor (IgnoreOne f f1 a) where
  fmap fun (IgnoringSomething fa gb) = IgnoringSomething fa (fmap fun gb)

-- ex.8
data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance (Functor g) => Functor (Notorious g o a) where
  fmap fun (Notorious go ga gt) = Notorious go ga (fmap fun gt)

-- ex.9
data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- ex.10
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats g1 g2 g3) =
    MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

-- ex.11
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read (f . g)
