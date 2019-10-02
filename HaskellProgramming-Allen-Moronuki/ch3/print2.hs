module Print2 where

main :: IO()
main = do
  putStrLn "Count four for me: "
  putStr "one, two"
  putStrLn ", three, and "
  putStrLn "four!"
