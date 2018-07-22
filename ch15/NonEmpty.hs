data NonEmpty a = a :| [a] deriving (Eq, Ord, Show)

-- Does not have an identity value since it can´t be empty 
