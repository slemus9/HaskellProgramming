import Data.Char

-- Examples
f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
 print a
 return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

-- ex.1
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf xs ys = go xs ys ys where
  go _ [] _ = False
  go [] _ _ = True
  go s@(x:xs) (y:ys) word
   | (x == y) = True && go xs word word
   | otherwise = go s ys word

-- ex.2
capitalizeWords :: String -> [(String, String)]
capitalizeWords s = splitIntoTuples (words s) where
  capitalizeWord s@(x : xs)= (s, (toUpper x) : xs)
  splitIntoTuples = map (capitalizeWord)
