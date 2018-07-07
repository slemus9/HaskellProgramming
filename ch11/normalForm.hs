data Fiction = Fiction deriving Show
data NonFiction = NonFiction deriving Show

data BookType = FictionBook Fiction | NonFictionBook NonFiction
 deriving Show

type AuthorName = String

-- data Author = Author (AuthorName, BookType)
-- Normal Form <=> Sum of products
-- In Normal Form: (No further evaluation)

data Author = Fiction AuthorName |
 NonFiction AuthorName deriving (Eq, Show)

data Expr =
  Number Int
  | Add Expr Expr
  | Minus Expr
  | Mult Expr Expr
  | Divide Expr Expr

-- Alternative:
type Number1 = Int
type Add1 = (Expr1, Expr1)
type Minus1 = Expr1
type Mult1 = (Expr1, Expr1)
type Divide1 = (Expr1, Expr1)

type Expr1 =
  Either Number1
   (Either Add1
      Either Minus1
          Either Mult1 Divide1)
