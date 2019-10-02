module Arith4 where

-- id :: a -> a
-- id x = x
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- ex.5 Write a PointFree version for roundTrip:
roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

-- ex.6
roundTripEx6 :: (Show a, Read b) => a -> b
roundTripEx6 a = (read . show) a

main = do
  print (roundTripEx6 4 :: Int)
  print (id 4)
