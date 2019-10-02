{-# LANGUAGE InstanceSigs #-}

import Data.Char
import Data.List

data DaPhone =
  DaPhone [(Char, [Char])] deriving Show

myPhone  = DaPhone
  [('1', ""), ('2', "abc"), ('3', "def"),
   ('4', "ghi"), ('5', "jkl"), ('6', "mno"),
   ('7', "pqrs"), ('8', "tuv"), ('9', "wxyz"),
   ('*', "^"), ('0', "+_"), ('#', ".,")]

convo :: [String]
convo =
   ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol lol",
   "Lol ya",
   "Wow ur cool haha. ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Haha thanks just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char
type Presses = Int

getCharPos :: Char -> [Char] -> Int
getCharPos c xs = go c xs 0 where
 go _ [] _ = -1
 go c (x : xs) count
   | c == x = count
   | otherwise = go c xs (count + 1)

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone xs) c = getTaps xs c where
 getTaps [] c = [(c, 0)]
 getTaps t@((d, ls) : xs) c
  | c == d = [(d, (length ls + 1))]
  | isUpper c = ('*', 1) : getTaps t (toLower c)
  | charPos /= -1 = [(d, charPos + 1)]
  | otherwise = getTaps xs c
  where
    charPos = getCharPos c ls

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone ws = concat . map (\c -> reverseTaps phone c) $ ws

-----------------------------------------------------------------

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\ (d,p) p' -> p + p' ) 0

-----------------------------------------------------------------
{-
type CharTable = [(Char, Int)]

searchTable :: CharTable -> Char -> (Char, Int)
searchTable [] c = (c,0)
searchTable ((x, i) : xs) c =
  if c == x then (x, i) else searchTable xs c
-}

countInstance :: (Eq a) => [a] -> a -> Int
countInstance xs x = foldr (\ y acc -> if x == y then acc + 1 else acc) 0 xs

countLetter :: String -> Char -> Int
countLetter xs c = countInstance xs c

mostPopularLetter :: String -> Char
mostPopularLetter s@(x : xs) =
  foldr (\ c c' -> if countLetter s c > countLetter s c' then c else c') x xs

letterCost :: DaPhone -> Char -> Int
letterCost phone c = fingerTaps $ reverseTaps phone c

coolestLtr :: [String] -> Char
coolestLtr s = mostPopularLetter $ filter (\c -> c /= ' ') (concat s)

countWord :: String -> String -> Int
countWord xs w = countInstance (words xs) w

mostPopularWord :: String -> String
mostPopularWord sentence =
  foldr (\ w w' -> if countWord sentence w > countWord sentence w' then w else w') (head ls) (tail ls)
  where
    ls = removeDots (words sentence)

removeDots :: [String] -> [String]
removeDots = map (\w -> if w !! (length w - 1) == '.' then take (length w - 1) w else w )

coolestWord :: [String] -> String
coolestWord s = mostPopularWord $ (concat . intersperse " ") s
