module Ch9.Calculator(

) where

import Ch9.Notes
import Ch8.ArithmeticParser

-- * Calculator
box :: [String ]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: [Char]
buttons = standard ++ extra where
            standard = "qcd=123+456-789*0()/"
            extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [writeat (1, y) xs | (y, xs) <- zip [1 .. 13] box]

display :: String -> IO ()
display xs = writeat (3, 2) "             " >>= \_ ->
             writeat (3, 2) (reverse $ take 13 (reverse xs))

calc :: String -> IO ()
calc xs = display xs >>= \_ ->
          getChar    >>= \c ->
          (if elem c buttons then
            process c xs
           else
            showError "character not allowed" >>= \_ ->
            calc xs)

process :: Char -> String -> IO ()
process c xs
  | elem c "qQ\ESC" = quit
  | elem c "dD\BS\DEL" = delete xs
  | elem c "=\n" = eval xs
  | elem c "cC" = clear
  | otherwise = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
                [(n, "")]  -> calc (show n)
                [(_, out)] -> showError ("unconsummed input " ++ out) >>= \_ ->
                              calc xs
                []         -> showError "invalid input" >>= \_ ->
                              calc xs

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = cls     >>= \_ ->
      showbox >>= \_ ->
      clear

-- Exercise 2
showError :: String -> IO ()
showError out = writeat (1, 14) ("Error: " ++ out)
