data Example = MakeExample deriving Show
data Example1 = MakeExample1 Int deriving Show

-- Datatypes that only contain a unary constructor always have the
--  same cardinality as the type they contain
-- Goats has the same number of inhabitants as Int
-- Unary constructors are the identity function for cardinality
data Goats = Goats Int deriving (Eq, Show)
