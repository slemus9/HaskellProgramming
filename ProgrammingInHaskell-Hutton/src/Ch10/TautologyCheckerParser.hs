module Ch10.TautologyCheckerParser(

) where

import Ch8.Notes8Alt (Parser, symbol, (+++), upper, parse)
import Ch9.Notes9 (cls)
import Ch10.TautologyChecker (Prop(Const, Var, Not, And, Imply, Or, Equiv),
                              isTaut)

-- Exercise 6

{- The language to construct:

    prop := term (=> prop | <=> prop | \epsilon)
    term := factor (∧ term | ∨ term | \epsilon)
    factor := (¬ | \epsilon) atom
    atom := (prop) | var | bool
    var  := A | B | C | ... | Z
    bool := True | False
-}
prop :: Parser Prop
prop =  term >>= \t ->
        (symbol "=>" >>= \_ ->
         prop        >>= \p ->
         return (Imply t p))
        +++
        (symbol "<=>" >>= \_ ->
         prop         >>= \p ->
         return (Equiv t p))
        +++
        return t 

term :: Parser Prop
term =  factor >>= \f ->
        (symbol "&&" >>= \_ ->
         term        >>= \t ->
         return (And f t))
        +++
        (symbol "||" >>= \_ ->
         term        >>= \t ->
         return (Or f t))
        +++
        return f

factor :: Parser Prop
factor = (atom >>= \a ->
          return a)
         +++
         (symbol "!" >>= \_ ->
          atom       >>= \a ->
          return (Not a))

atom :: Parser Prop
atom = (symbol "(" >>= \_ ->
        prop       >>= \p ->
        symbol ")" >>= \_ ->
        return p)
       +++
       (var        >>= \v ->
        return v)
       +++
       (bool       >>= \b ->
        return b)


var :: Parser Prop
var = upper >>= \v ->
      return (Var v)

bool :: Parser Prop
bool = (symbol "True" >>= \_ ->
        return (Const True))
       +++
       (symbol "False" >>= \_ ->
        return (Const False))

evalprop :: String -> String
evalprop xs = case parse prop xs of
                   [(p, "")]   -> if isTaut p then "It's a Tautology! :D ." else "It's not a Tautology D: ."
                   [(_, out)] -> "Error: Unconsumed input: " ++ out
                   []         -> "Error: Invalid input"

displayinstructions :: IO ()
displayinstructions = putStrLn "Enter a proposition you would like to check of it's a tautology, follow the following rules:"
                            >>= \_ ->
                      putStrLn "1. The variables can only be upper case characters (A to Z) and can be accompanied by parenthesis. You can also enter the constants True and False."
                            >>= \_ ->
                      putStrLn "2. You are only allowed to use the following symbols: ! (for negation), && (for and), || (for or), => (for implies) and <=> (for equivalence)."
                            >>= \_ ->
                      putStrLn "3. Press Enter to evaluate the proposition."
                            >>= \_ ->
                      putStrLn "Enter the proposition: "

runcheck :: IO ()
runcheck = cls                 >>= \_    -> 
           displayinstructions >>= \_    -> 
           getLine             >>= \prop -> 
           putStrLn ("\n" ++ evalprop prop)