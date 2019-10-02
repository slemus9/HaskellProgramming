-- Chapter 8 exercise
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"

-- ex.1 Result: "woops mrow woohoo!"

-- ex.2 Result: "1 mrow haha"

-- ex.3 Result: "woops mrow 2 mrow haha"

-- ex.4 Result: "woops mrow blue mrow haha"

-- ex.5 Result: "pink mrow haha mrow green mrow woops mrow blue"

-- ex.6 Result: "are mrow Pugs mrow awesome"
