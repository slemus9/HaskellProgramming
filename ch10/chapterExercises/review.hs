-- Chapter 10 exercise
-- ex.1
stops = "pbtdkg"
vowels = "aeiou"

nouns = ["Sebastian", "Santiago", "Christian", "Apple", "Pizza", "Haskell"]
verbs = ["eats", "encourages", "mentions", "likes", "loves"]

lists3Combs :: [a] -> [b] -> [(a, b, a)]
lists3Combs xs ys = [(x, y, z) | x <- xs, y <- ys, z <- xs]

lists3CombsP :: String -> String -> [(Char, Char, Char)]
lists3CombsP xs ys = [(x, y, z) | x <- xs, y <- ys, z <- xs, x == 'p']
