import Notes8Alt
import Data.Char

-- 1. Define the parser int :: Parser Int.
int :: Parser Int
int = (char '-' >>= \_ ->
      natural >>= \num ->
      return (-num))
      +++
      natural

{- 2. Define a parser comment :: Parser () for ordinary Haskell comments
that begin with "--" and end with '\n'
-}
comment :: Parser ()
comment = symbol "--" >>= \_ ->
          many' (sat (/= '\n')) >>= \_ ->
          return ()

{- 3. Using our second grammar for arithmetic expressions, draw the two pos-
sible parse trees for the expression 2 + 3 + 4.

1)
  expr( expr( expr(factor(term(nat(2)))), +, expr(factor(term(nat(3)))) ), +, expr(factor(term(nat(4)))) )

2)
  expr( expr(factor(term(nat(2))), +, expr( expr(factor(term(nat(3))), +, expr(factor(term(nat(4))) ))

-}

{- 4. Using our third grammar for arithmetic expressions, draw the parse trees
for the expressions 2 + 3, 2 ∗ 3 ∗ 4 and (2 + 3) + 4.

For 2 + 3:
  expr( term(factor(nat(2))), +, expr(term(factor(nat(3)))) )

For 2 * 3 * 4
  expr( term( factor(nat(2)), *, term( factor(nat(3)), *, term(factor(nat(4))) ) ) )

For (2 + 3) + 4
  expr( expr(term(factor( (, expr( expr(term(factor(nat(2)) ,) ), +,
        expr(term(factor(nat(2)))) )) )) ), +, expr(term(factor(nat(4)))))
-}

{- 5. Explain why the final simplification of the grammar for arithmetic expressions
has a dramatic effect on the efficiency of the resulting parser.

Without the simplification the recursion over expresion would be to big, giving an exponetial
complexity. For an expresion with just a number, the recursion, as we saw in the previous examples,
has 4 steps.
-}

{- 6. Extend the parser for arithmetic expressions to support subtraction and
division, based upon the following extensions to the grammar:

  expr ::= term(+ expr | - expr | \epsilon)
  term ::= factor(* term | / term | \epsilon)
-}
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

eval :: String -> Int
eval xs = case parse expr xs of
               [(n, [])] -> n
               [(_, out)] -> error ("unconsummed input " ++ out)
               [] -> error ("invalid input")

{- 7. Further extend the grammar and parser for arithmetic expressions to
support exponentiation, which is assumed to associate to the right and
have higher priority than multiplication and division, but lower priority
than parentheses and numbers.

Extension:
  expr ::= term(+ expr | - expr | \epsilon)
  term ::= factor(* term | / term | \epsilon)
  factor ::= base(^ factor | \epsilon)
  base ::= (expr) | nat
  nat ::= 0 | 1 | 2 ...
-}
base :: Parser Int
base = (symbol "(" >>= \_ ->
        expr       >>= \e ->
        symbol ")" >>= \_ ->
        return e)
       +++
       natural

factor :: Parser Int
factor = base >>= \b ->
         (symbol "^" >>= \_ ->
          factor     >>= \f ->
          return (b ^ f))
         +++
         return b
