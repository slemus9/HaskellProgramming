import Ch9.Notes9
-- * Game of life
type Pos = (Int, Int)
type Board = [Pos]

width :: Int
width = 5

height :: Int
height = 5

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

showcells :: Board -> IO ()
showcells b = seqn [writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b = not.(isAlive b)

neighbs :: Pos -> [Pos]
neighbs (x, y) = map wrap [(x - 1, y - 1), (x, y - 1),
                           (x + 1, y - 1), (x - 1, y),
                           (x + 1, y), (x - 1, y + 1),
                           (x, y + 1), (x + 1, y + 1)]

wrap :: Pos -> Pos
wrap (x, y) = (((x - 1) `mod` width) + 1,
               ((y - 1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length.(filter (isAlive b)).neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2, 3]]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups(filter (/= x) xs)

births :: Board -> [Pos]
births b = [p | p <- rmdups(concat(map neighbs b)),
                isEmpty b p, liveneighbs b p == 3]

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = cls         >>= \_ ->
         showcells b >>= \_ ->
         wait 10000   >>= \_ ->
         life (nextgen b)

wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1 .. n]]

-- Exercise 3

shownewcells :: Board -> Board -> IO ()
shownewcells bprev bpost = seqn [writeat p1 "O" >>= \_ ->
                                 writeat p2 " "
                                 | p1 <- filter (\x -> not(elem x bprev)) bpost,
                                   p2 <- filter (\x -> not(elem x bpost)) bprev]

lifenoflick :: Board -> IO ()
lifenoflick b = cls >>= \_ ->
                go [(width + 1, height + 1)] b
  where
    go :: Board -> Board -> IO()
    go bprev bpost = shownewcells bprev bpost >>= \_ ->
                     wait 5000                >>= \_ ->
                     go bpost (nextgen bpost)
