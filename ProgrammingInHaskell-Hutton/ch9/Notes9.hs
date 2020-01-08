module Notes9 (
  seqn,
  writeat,
  beep,
  cls,
  goto
) where

-- ** Interactive Programs

-- * The input/output Type
{- An interactive program can be viewed as a pure function that takes the
current state of the "world" as its argument, and returns a modified state
of the world. Additionaly, it might produce a result value besides producing
side effects.

  type IO a = World -> (a, World)

In reality, Haskell systems don't  pass the entirety of the World's state to
improve efficiency.

Examples of simple actions:

  getChar :: IO Char
  putChar :: Char -> IO ()
  return :: a -> IO a
-}

-- * Sequencing
{-
  (>>=) :: IO a -> (a -> IO b) -> IO b
  f >>= g = \world -> case f world of
                        (v, world') -> g v world'
 -}

-- * Derived Primitives
getLine' :: IO String
getLine' = getChar >>= \x ->
            (if x == '\n'
              then return []
             else
              getLine' >>= \xs ->
              return (x : xs))

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >>= \_ ->
                   putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = putStr' xs >>= \_ ->
               putChar '\n'

strlen :: IO ()
strlen = putStr' "Enter a string: " >>= \_ ->
         getLine'                   >>= \xs ->
         putStr' "The string has "  >>= \_ ->
         putStr' (show (length xs)) >>= \_ ->
         putStrLn' " characters"

beep :: IO ()
beep = putStr' "\BEL"

cls :: IO()
cls = putStr' "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr' ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = goto p >>= \_ ->
               putStr' xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a : as) = a >>= \_ ->
                seqn as

putStr'' xs = seqn [putChar x | x <- xs]
