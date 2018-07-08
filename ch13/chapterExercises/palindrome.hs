import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower)

toLowerString :: String -> String
toLowerString = map (\c -> toLower c)

lowerAndConcat :: String -> String
lowerAndConcat s = helper (words s) where
  helper = concat . foldr (\w rest -> toLowerString w : rest) []


palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let wds = lowerAndConcat line1
  case (wds == reverse wds) of
    True -> putStrLn "It´s a palindrome!"
    False -> exitSuccess
