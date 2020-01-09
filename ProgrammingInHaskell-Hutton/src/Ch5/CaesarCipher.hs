module Ch5.CaesarCipher(

) where

import Data.Char
import Ch5.Notes5

-- * Caesar's Cipher
-- Only for lower case letters

{- Converts a lower-case letter between 'a' and 'z'
into the corresponding integer between 0 and 25
-}
let2int :: Char -> Int
let2int c = ord c - ord 'a'

{- Performs the opposite conversion of let2int
-}
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

{- Shifts a character n positions (n can be positive or
negative)
-}
shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- * Frequency Tables

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

{- Frequency table for any string.
Outputs a table with the frequencies of the characters
(in lower case) within the string.

Note that the local definition n = lowers xs, ensures that
the number of lower case letters in the argument string is
calculated only once.
-}
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
            where n = lowers xs

-- * Cracking the cipher

{- A standard method for comparing a list of observed frequencies
os with a list of expected frequencies es is the chi-square statistic:
  \sum_{i=0}^{n-1} (os_i - es_i)^2 / es_i

The smaller the value it produces, the better match between the two
frequency list.
-}
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o,e) <- zip os es]

{- rotate is a function that rotates the elements of a list n
places to the left, wrapping around at the start of the list.
-}
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

{- The table of expected frequencies for each character
of the English language is the following:
-}
table :: [Float ]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
        6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

{- To determine the shift factor, usually we can do the following:
    - Produce the frequency table of the encoded string.
    - Calculate the chi-square statistic for each possible rotation
      of this table with respect to the table of expected frequencies.
    - Use the position of the minimum chi-square value as the shift factor.
-}
crack :: String -> String
crack xs = encode (-factor) xs
            where
              factor = head (positions(minimum chitab) chitab)
              chitab = [chisqr (rotate n table') table | n <- [0..25]]
              table' = freqs xs
