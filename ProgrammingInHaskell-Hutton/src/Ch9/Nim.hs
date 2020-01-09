import Data.Array
import Ch9.Notes9

-- Exercise 6
type Array1D a = Array Int a
type Board = Array1D Int

height :: Int
height = 5

board :: Board
board = array (1, height) [ (i, height + 1 - i) | i <- [1 .. height]]

removestars :: Int -> Int -> Board -> Board
removestars i n b = b // [(i, if n > e then 0 else e - n)]
                    where e = b ! i

writestars :: Int -> Board -> String
writestars i b = replicate (b ! i) '*'

showboard :: Board -> IO ()
showboard b = seqn [writeat (1, i) ( (show i) ++ ": " ++ writestars i b)
                      | i <- [1 .. height]]

showmessage :: Int -> String -> IO ()
showmessage i xs = writeat (1, i) xs

gamefinished :: Board -> Bool
gamefinished = foldlarr (\acc x -> x == 0 && acc) True

quit :: IO ()
quit = goto (1, height + 4)

nim :: Board -> IO ()
nim b = go 1 b
  where
    go :: Int -> Board -> IO ()
    go player b = cls         >>= \_ ->
                  showboard b >>= \_ ->
                  showmessage (height + 1) ("Player " ++ show player ++ ": enter the row followed by a space and the number of stars you want to delete:\n")
                              >>= \_ ->
                  getLine     >>= \p ->
                  (let
                      pair = words p
                      i = read $ head pair
                      n = read $ head (tail pair)
                      newb = removestars i n b
                   in
                      showboard newb >>= \_ ->
                      if gamefinished newb then
                        showmessage (height + 2) ("Player " ++ show player ++ " won the game") >>= \_ ->
                        quit
                      else
                        go (if player == 1 then 2 else 1) newb)

-- based on https://www.twanvl.nl/blog/haskell/four-ways-to-fold
lo :: Array1D a -> Int
lo = fst . bounds

hi :: Array1D a -> Int
hi = snd . bounds

foldlarr :: (b -> a -> b) -> b -> Array1D a -> b
foldlarr f z xs = go z (lo xs)
  where
    go z i
      | i > hi xs = z
      | otherwise = go (f z (xs ! i)) (i + 1)
