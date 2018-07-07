import Data.List
-- example GHCi session above the functions

-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe s
 | s == "the" = Nothing
 | otherwise = Just s

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe s = (concat . intersperse " ") (helper (words s)) where
  helper [] = []
  helper (x : xs) =
    case notThe x of
      Nothing -> "a" : helper xs
      Just y -> y : helper xs

checkVowel :: Char -> Bool
checkVowel c = elem c "aeiou" || elem c "AEIOU"


-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = helper (words s) where
  helper [] = 0
  helper (x : xs) =
    case notThe x of
      Nothing -> isFollowedByVowel + helper xs
      Just y -> helper xs
      where
        isFollowedByVowel = if (checkVowel . head . head) xs then 1 else 0


-- >>> countVowels "the cow"
-- 2
-- >>> countVowels "Mikolajczak"
-- 4
countVowels :: String -> Integer
countVowels [] = 0
countVowels (x : xs)
  | checkVowel x = 1 + countVowels xs
  | otherwise = countVowels xs

-----------------------------------------------------------

newtype Word' = Word' String deriving (Eq, Show)

countConsonats :: String -> Integer
countConsonats = foldr ( \c acc -> if not (checkVowel c) then 1 + acc else acc) 0

mkWord :: String -> Maybe Word'
mkWord s =
  case countConsonats s < countVowels s of
    True -> Nothing
    False -> Just (Word' s)
    where
      numVowels = countVowels s
