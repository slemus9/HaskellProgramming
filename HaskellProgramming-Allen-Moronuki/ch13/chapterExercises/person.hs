type Name = String
type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid =
  NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                          "Name was: " ++ show name ++
                          " Age was: " ++ show age

checkPerson :: Either PersonInvalid Person -> IO ()
checkPerson (Right p) =
  putStrLn ("Yay! Successfully got a person: " ++ show p)
checkPerson (Left e) =
  putStrLn ("Could not create the person: " ++ show e)


gimmePerson :: IO ()
gimmePerson = do
  putStr "Enter the person's name: "
  name <- getLine
  putStr "Enter the person's age: "
  a <- getLine
  let age = read a :: Integer
  let person = mkPerson name age
  checkPerson person
