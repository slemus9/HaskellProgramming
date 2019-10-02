data Rocks = Rocks String deriving (Eq, Show)
data Yeah = Yeah Bool deriving (Eq, Show)
data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- ex.1 It does not typecheck; missing some data constructors
{-
phew = Papu "chases" True
Correction: -}
phew = Papu (Rocks "chases") (Yeah True)

-- ex.2 It does typecheck
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- ex.3 It does typecheck
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- ex.4 It does not typecheck since Papu doesn´t have an Ord instance
{-
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
-}
