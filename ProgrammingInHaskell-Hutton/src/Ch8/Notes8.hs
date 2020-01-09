import Data.Char

-- ** Functional Parsers

{-
We can define naturally a parser as a mapping from a String to a Tree:
  type Parser = String -> Tree

However, sometimes the parser might not always consume the entire String.
To represent this, we can store the un-consumed part of the argument in
another String:
  type Parser = String -> (Tree, String)

Moreover, we have to handle successful and unsuccessful executions. To do
that, we can generate a list of results, in wich an empty list denotes
failure:
  type Parser = String -> [(Tree, String)]

Finally, since there are different kinds of trees, we can have a generic
definition like the following:
-}
type Parser a = String -> [(a, String)]

-- * Basic Parsers

-- return v: always succeeds with the result value v, without consuming any input string.
return' :: a -> Parser a
return' v = \inp -> [(v, inp)]

-- failure: always fails , regardless of the input string.
failure :: Parser a
failure = \inp -> []

-- item: fails if the input string is empty or succeeds with the first character otherwise.
item :: Parser Char
item = \inp -> case inp of
                    [] -> []
                    (x : xs) -> [(x, xs)]

-- application function:
parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

-- * Sequencing

-- Sequencing operator >>= (read as "then").
{- A typical parser built using >>= has the following structure:
  p1 >>= \v1 ->
  p2 >>= \v2 ->
  .
  .
  .
  pn >>= \vn ->
  return (f v1 v2 .. vn)

The execution can be read as follows:
  "apply p1 and call its result v1; then
   apply p2 and call its result v2; then
   .
   .
   .
   apply pn and call its result vn; then (finally)
   combine all the results into a single value applying f"

Haskell offers the following sugar syntax for this execution:
  do  v1 <- p1
      v2 <- p2
      .
      .
      .
      vn <- pn
      return (f v1 v2 .. vn)

As with list comprehensions, the expression vi <- pi is called
a Generator.

NOTE: "do" notation is considered harmful: https://wiki.haskell.org/Do_notation_considered_harmful
-}
(>>==) :: Parser a -> (a -> Parser b) -> Parser b
p >>== f = \inp -> case parse p inp of
                      [] -> []
                      [(v, out)] -> parse (f v) out

{- A parser that consumes three characters, discards the second,
and returns the first and third as a pair:
-}
p3c :: Parser (Char, Char)
p3c = item >>== \x ->
    item >>== \_ ->
    item >>== \y ->
    return' (x, y)

-- * Choice

{- Another way to combine parsers is: apply the first parser to
the input and, if this fails, apply the second instead.

To accomplish this combination, we define the choice operator +++
(read as "or else"):
-}
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
                        [] -> parse q inp
                        [(v, out)] -> [(v, out)]

-- * Derived Primitives (parsing primitives)

-- Parser for single characters that satisfy the predicate p.
sat :: (Char -> Bool) -> Parser Char
sat p = item >>== \x ->
        if p x then return' x else failure

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return' []
string (x : xs) = char x >>== \_ ->
                  string xs >>== \_ ->
                  return' (x : xs)

{- many and many1 apply a parser p as many times
as possible until it fails. Many allows 0 or more
applications of p, whereas many 1 requires at least
one successful application.
-}
many :: Parser a -> Parser [a]
many p = many1 p +++ return' []

many1 :: Parser a -> Parser [a]
many1 p = p      >>== \v  ->
          many p >>== \vs ->
          return' (v : vs)

ident :: Parser String
ident = lower >>== \x ->
        many alphanum >>== \xs ->
        return' (x : xs)

nat :: Parser Int
nat = many1 digit >>== \xs ->
      return' (read xs)

-- () : empty tuple. NOTE: Not working correctly.
space :: Parser ()
space = many (sat isSpace) >>= \_ ->
        return' ()

-- * Handling Spacing

-- Primitive that ignores any space before and after applying a parser for a token
token :: Parser a -> Parser a
token p = space >>== \_ ->
          p     >>== \v ->
          space >>== \_ ->
          return' v

-- Parsers that ignore spacing:
identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

{- A parser for a non-empty list of natural numbers that ignores spacing
around tokens can be defined as follows.
NOTE: Not working correctly.
-}
pIntList :: Parser [Int]
pIntList =  symbol "["     >>== \_  ->
            natural        >>== \n  ->
            many (symbol "," >>== \_ ->
                  natural) >>== \ns ->
            symbol "]"     >>== \_  ->
            return' (n : ns)

-- * Arithmetic Expressions

{-
expr ::= term (+ expr | e)
term ::= factor (* term | e)
factor ::= (expr) | nat
nat ::= 0 | 1 | 2 | ...
where e denotes empty string
-}
expr :: Parser Int
expr = term >>== \t ->
        (symbol "+" >>== \_ ->
         expr       >>== \e ->
         return' (t + e))
        +++ return' t

term :: Parser Int
term = factor >>== \f ->
        (symbol "*" >>== \_ ->
         term       >>== \t ->
         return' (f * t))
        +++ return' f

factor :: Parser Int
factor = (symbol "(" >>== \_ ->
          expr       >>== \e ->
          symbol ")" >>== \_ ->
          return' e)
         +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
               [(n, [])]  -> n
               [(_, out)] -> error ("unconsumed input " ++ out)
               []         -> error "invalid input"
