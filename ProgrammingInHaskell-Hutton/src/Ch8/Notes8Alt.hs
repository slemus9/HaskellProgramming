module Ch8.Notes8Alt(
  Parser,
  parse,
  (+++),
  natural,
  symbol,
  sat,
  alphanum,
  many',
  char
) where

import Data.Char
import Control.Applicative
import Control.Monad

-- http://www.cs.nott.ac.uk/~pszgmh/Parsing.lhs
-- ** Functional Parsers
newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Functor Parser where
  fmap f par = P (\inp -> case parse par inp of
                                [] -> []
                                [(v, out)] -> [(f v, out)])

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

instance Monad Parser where
  return v = P (\inp -> [(v, inp)])
  p >>= f = P (\inp -> case parse p inp of
                          [] -> []
                          [(v, out)] -> parse (f v) out)

instance MonadPlus Parser where
  mzero = P (\_ -> [])
  p `mplus` q = P (\inp -> case parse p inp of
                        [] -> parse q inp
                        [(v, out)] -> [(v, out)])

-- * Basic Parsers
failure :: Parser a
failure = mzero

item :: Parser Char
item = P (\inp -> case inp of
                    [] -> []
                    (x : xs) -> [(x, xs)])

-- * Choice
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q

-- * Derived Primitives
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

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
string [] = return []
string (x : xs) = do char x
                     string xs
                     return (x : xs)

many' :: Parser a -> Parser [a]
many' p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do v <- p
             vs <- many' p
             return (v : vs)

ident :: Parser String
ident = do x <- lower
           xs <- many' alphanum
           return (x : xs)

nat :: Parser Int
nat = do xs <- many1 digit
         return (read xs)

space :: Parser ()
space = many (sat isSpace) >>= \_ ->
        return ()

-- * Handling spacing
token :: Parser a -> Parser a
token p = space >>= \_ ->
          p     >>= \v ->
          space >>= \_ ->
          return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

pIntList :: Parser [Int]
pIntList = symbol "["      >>= \_ ->
           natural         >>= \n ->
           many' (symbol "," >>= \_ ->
                  natural) >>= \ns ->
           symbol "]"      >>= \_ ->
           return (n : ns)

-- * Arithmetic Expressions
expr :: Parser Int
expr = term >>= \t ->
        (symbol "+" >>= \_ ->
         expr       >>= \e ->
         return (t + e))
        +++ return t

term :: Parser Int
term = factor >>= \f ->
        (symbol "*" >>= \_ ->
         term       >>= \t ->
         return (f * t))
        +++ return f

factor :: Parser Int
factor = (symbol "(" >>= \_ ->
         expr >>= \e ->
         symbol ")" >>= \_ ->
         return e)
         +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
               [(n, [])] -> n
               [(_, out)] -> error ("unconsummed input " ++ out)
               [] -> error ("invalid input")
