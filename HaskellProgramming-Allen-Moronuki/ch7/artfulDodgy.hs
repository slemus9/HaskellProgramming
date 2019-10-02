dodgy :: Num a => a -> a -> a
oneIsOne :: Num a => a -> a
oneIsTwo :: Num a => a -> a

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2
