import Data.Char
-- Chapter 9 exercise

-- ex.2
filterUpper :: String -> String
filterUpper s = filter (\x -> isUpper x) s

-- ex.3
capitalize :: String -> String
capitalize "" = ""
capitalize (x : xs) = toUpper x : xs

-- ex.4
capitalizeString :: String -> String
capitalizeString "" = ""
capitalizeString (x : xs) = toUpper x : capitalizeString xs

-- ex.5
capitalizeAndReturn :: String -> Char
capitalizeAndReturn s = (head . capitalize) s
