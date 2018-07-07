--data Person = MkPerson String Int deriving (Eq, Show)
-- In Record Syntax:
data Person =
  Person { name :: String,
           age :: Int
         } deriving (Eq, Show)

jm = Person "Julie" 108
ca = Person "Chris" 16

{-
jm = MkPerson "Julie" 108
ca = MkPerson "Chris" 16

namae :: Person -> String
namae (MkPerson s _) = s
-}
