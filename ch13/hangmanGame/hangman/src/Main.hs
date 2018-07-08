module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- [1] the word we’re trying to guess
-- [2] the characters we’ve filled in so far
-- [3] the letters we’ve guessed so far
data Puzzle = Puzzle String [Maybe Char] [Guess Char]
--                     [1]     [2]         [3]

data Guess a = Correct a | Incorrect a

instance (Show a) => Show (Guess a) where
  show (Correct g) = show g
  show (Incorrect g) = show g

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ "Guessed so far: " ++ (show guessed)

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (nothingList) [] where
  nothingList = map (const Nothing) s

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = elem c s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ gs) c = elemInGuessed c gs where
  elemInGuessed _ [] = False
  elemInGuessed x ((Correct g) : xs) = (x == g) || elemInGuessed x xs
  elemInGuessed x ((Incorrect g) : xs) = (x == g) || elemInGuessed x xs

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter p@(Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar newGuess where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
      then Just wordChar
      else guessChar

    newFilledInSoFar =
      zipWith (zipper c) word filledInSoFar

    newGuess =
      if charInWord p c
      then (Correct c) : s
      else (Incorrect c) : s

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_ , True) -> do
      putStrLn "You already guessed that\
                  \ character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word,\
                  \ filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in\
                  \ the word, try again."
      return (fillInCharacter puzzle guess)

isCorrect :: Guess Char -> Bool
isCorrect (Correct _) = True
isCorrect _ = False

getNumIncorrect :: [Guess Char] -> Int
getNumIncorrect =
  foldr (\x acc -> if (not . isCorrect) x then acc + 1 else acc) 0

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (getNumIncorrect guessed) > 7 then
    do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 10

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw) where
    gameLength w =
      let l = length (w :: String)
      in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0 , (length wl) - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr " Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStr "Your guess must\
                      \ be a single character"

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
