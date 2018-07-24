-- ex.1 a = (+1) $ read "[1]" :: [Int]
a = fmap (+1) (read "[1]" :: [Int])

-- ex.2 b = (++ "lol") (Just ["Hi,", "Hello"])
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- ex.3 c = (*2) (\x -> x - 2)
c = fmap (*2) (\x -> x - 2)

-- ex.4 d = ((return '1' ++) . show) (\x -> [x, 1..3])
-- d = d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])
d = (return '1' ++) . show . (\x -> [x, 1..3])

-- ex.5
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read <$> ("123" ++) <$> show <$> ioi
    in fmap (*3) changed
