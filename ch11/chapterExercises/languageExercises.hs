import Data.Char

-- Only works for english chars (Unicode)
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x : xs)
 | (ord x >= 97) && (ord x <= 122) = (toUpper x) : xs
 | otherwise = x : capitalizeWord xs

splitBy :: String -> Char -> [String]
splitBy [] _ = [""]
splitBy (x: xs) c
 | x == c = [c] : rest
 | otherwise = (x : head rest) : tail rest
 where
   rest = splitBy xs c

capitalizeParagraph :: String -> String
capitalizeParagraph s = concatenate (splitBy s '.') where
  concatenate = concat . map (capitalizeWord)
