module Ch8.ArithmeticParser(
    parse,
    expr
) where

import Ch8.NotesAlt
-- * Arithmetic Expressions

expr :: Parser Int
expr = term >>= \t ->
        (symbol "+" >>= \_ ->
         expr       >>= \e ->
         return (t + e))
        +++
        (symbol "-" >>= \_ ->
         expr       >>= \e ->
         return (t - e))
        +++
        return t

term :: Parser Int
term = factor >>= \f ->
        (symbol "*" >>= \_ ->
         term       >>= \t ->
         return (f * t))
        +++
        (symbol "/" >>= \_ ->
         term       >>= \t ->
         return (f `div` t))
        +++
        return f

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