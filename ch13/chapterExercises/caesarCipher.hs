module Chipher where

import Data.Char

-- Caesar's cipher with right shift
-- Positions are Unicode chars
-- Unicode for 'a' is 97. Unicode for ' ' is 32. Unicode for 'A' is 65
shiftR :: Int -> Int -> Int
shiftR shift pos
 | pos == 32 = 32
 | pos >= 97 = ((pos + shift - 97) `mod` 26 ) + 97
 | pos >= 65 = ((pos + shift - 65) `mod` 26 ) + 65

deShiftR :: Int -> Int -> Int
deShiftR shift pos
 | pos == 32 = 32
 | pos >= 97 = ((pos - shift - 97) `mod` 26 ) + 97
 | pos >= 65 = ((pos - shift - 65) `mod` 26 ) + 65

cipherChar :: Char -> Int -> Char
cipherChar x shift = (chr . (shiftR shift) . ord) x

decipherChar :: Char -> Int -> Char
decipherChar x shift = (chr . (deShiftR shift) . ord) x

caesar :: String -> Int -> String
caesar "" _ = ""
caesar (x : xs) shift = cipherChar x shift : caesar xs shift

unCaesar :: String -> Int -> String
unCaesar "" _ = ""
unCaesar (x : xs) shift = decipherChar x shift : unCaesar xs shift


main :: IO ()
main = do
  putStr "Enter the word you want to cipher: "
  txt <- getLine
  putStr "Enter the number of shifts to the right: "
  s <- getLine
  let c = caesar txt (read s :: Int)
  putStrLn ("Your word is: " ++ c)
