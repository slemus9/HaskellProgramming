{- 1. Define an action readLine :: IO String that behaves in the same way as
getLine, except that it also permits the delete key to be used to remove
characters.
-}
back1 :: IO ()
back1 = putStr "\ESC[1D"

readLine :: IO String
readLine = go "" where
            go :: String -> IO String
            go cs = getChar >>= \c ->
                      (case c of
                        '\n'   -> return cs
                        '\DEL' -> if length cs == 0 then go cs
                                  else
                                    back1 >>= \_ ->
                                    back1 >>= \_ ->
                                    go (init cs)
                        _      -> go (cs ++ [c]))
{-- 2. Modify the calculator program to indicate the approximate position of
an error rather than just sounding a beep.
  SOLVED in file Calculator.hs
}

{-- 3. One some systems the game of life may flicker, due to the entire screen
being cleared each generation. Modify the game to avoid such flicker by
only redisplaying positions whose status changes.
  SOLVED in file GameOfLife.hs
}

{- 6. Nim Game
  SOLVED in file Nim.hs
-}
