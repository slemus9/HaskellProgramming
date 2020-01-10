module Ch10.TautologyCheckerParser(

) where

import Ch8.Notes8Alt (Parser, symbol, (+++), upper)
import Ch10.TautologyChecker (Prop(Const, Var, Not, And, Imply, Or, Equiv))

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
        var        >>= \v ->
        symbol ")" >>= \_ ->
        return v)
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