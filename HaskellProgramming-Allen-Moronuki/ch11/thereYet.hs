data ThereYet =
  There Integer Float String Bool deriving (Eq, Show)

nope :: Float -> String -> Bool -> ThereYet
noper = There 10

nopeYet :: String -> Bool -> ThereYet
nopeYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet "woohoo"

yusssss :: ThereYet
yusssss = notQuite False
