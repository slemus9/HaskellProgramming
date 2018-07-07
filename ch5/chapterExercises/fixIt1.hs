module Arith3Broken where

{- main :: IO ()
  Main = do
  print 1 + 2
  putStrLn 10
  print (negate -1)
  print ((+) 0 blah)
  where blah = negate 1
Correction: -}

main :: IO ()
main = do
  print (1 + 2)
  putStrLn (show 10)
  print (negate (-3))
  print ((+) 0 blah) where
    blah = negate 1
