data Product a b = a :&: b deriving (Eq, Show)

data List a = Nil | Cons a (List a)
