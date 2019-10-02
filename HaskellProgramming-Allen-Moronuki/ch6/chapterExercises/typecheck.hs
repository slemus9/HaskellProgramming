-- ex.1 Does not typecheck since person doesn´t have a Show instance
{-
data Person = Person Bool
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)
Correction: -}
data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- ex.2 It doesn't typecheck since it lacks from an Eq instance
{-
data Mood = Blah | Woot deriving (Eq, Show)
settleDown x = if x == Woot then Blah else x
Correction: -}

data Mood = Blah | Woot deriving (Eq, Show)
settleDown x = if x == Woot then Blah else x
-- ex.3 Answers to Questions:
{-
a) The only input values acceptable for settleDown function are
    the Mood data constructors
b) Throws an error since the type of the function == is Eq a => a -> a -> Bool ;
    meaning both parameters must bu of the same type
c) Throws an error since Mood doesn´t have an instance of Ord
-}

-- ex.4 It does typecheck 

type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)
s1 = Sentence "dogs" "droll"
s2 = Sentence "Julie" "loves" "dogs"
