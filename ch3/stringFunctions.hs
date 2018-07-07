removeHead s = drop 1 s

addExclamation s = s ++ "!"

returnFifthPos s = s !! 4

thirdLetter :: String -> Char
thirdLetter s = s !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome" !! x

rvrs :: String -> String
rvrs s = (drop 9 s) ++ " " ++ (take 2 (drop 6 s)) ++ " " ++ (take 5 s)
