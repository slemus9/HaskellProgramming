module Vigenere where

import Data.Char

-- Positions are Unicode chars
-- Unicode for 'a' is 97. Unicode for ' ' is 32. Unicode for 'A' is 65
txt = "MEET AT DAWN"
kwd = "ALLY"

keywordShiftList :: [Char] -> [Int]
keywordShiftList = map evaluateChar where
  evaluateChar c
   | ord c >= 97 = ord c - 97
   | ord c >= 65 = ord c - 65

textPositionList :: [Char] -> [Int]
textPositionList = map (\c -> ord c)

-- The first parameter is always the input to cipher
sumPositions :: (Int -> Int -> Int) -> [Int] -> [Int] -> [Int]
sumPositions f txt kwd = go txt kwd 0 where
  go [] _ _ = []
  go (x : xs) ys p
   | x == 32 = [32] ++ go xs ys p
   | otherwise = [add x (ys !! p)] ++ (go xs ys nextPos)
   where
     nextPos = (p + 1) `mod` (length ys)
     add x y
      | x >= 97 = (mod ((f x y) - 97) 26) + 97
      | x >= 65 = (mod ((f x y) - 65) 26) + 65

getPosList :: (Int -> Int -> Int) -> [Char] -> [Char] -> [Int]
getPosList f txt kwd =
  sumPositions f (textPositionList txt) (keywordShiftList kwd)

vigenere :: [Char] -> [Char] -> [Char]
vigenere txt kwd = map (\i -> chr i) (getPosList (+) txt kwd)

unVigenere :: [Char] -> [Char] -> [Char]
unVigenere txt kwd = map (\i -> chr i) (getPosList (-) txt kwd)



main :: IO ()
main = do
  putStr "Enter the word you want to cipher: "
  txt <- getLine
  putStr "Enter the keywod: "
  kwd <- getLine
  let c = vigenere txt kwd
  putStrLn ("Your word is: " ++ c)
