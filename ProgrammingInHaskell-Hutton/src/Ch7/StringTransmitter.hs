module Ch7.StringTransmitter(
  bin2int,
  int2bin,
  make8
) where

import Data.Char

-- ** String transmitter
{- Consider the problem of
simulating the transmission of a string using zeros and ones.

To simplify the definition of certain functions,  assume that
the binary numbers are written in reverse order. For instance,
the number 1101 would now be written as 1011
  1011 = (2^0 * 1) + (2^1 * 0) + (2^2 * 1) + (2^2 + 1)
-}

-- * Base conversion
-- type is used to declare a synonym
type Bit = Int

{- Transform a binary number (List of bits) into a base-10 number:
Here the library function iterate can be represented as:
  iterate f x = [x, f x, f (f x), f (f (f x)), ...]

This list is infinite, but due to lazy evaluation, it only produces
as many elements as required by the context. Given this function, we can
define the function bin2int as follows:
  bin2int :: [Bit] -> Int
  bin2int bits = sum [w * b | (w,b) <- zip weights bits]
                  where weights = iterate (*2) 1

There is, however, a much simplier way to define this function if we
consider the following algebraic reduction
  For a binary [a,b,c,d], the corresponding integer representation is:
    (1*a) + (2*b) + (4*c) + (8*d)
  = {simplifying 1*a}
    a + (2*b) + (4*c) + (8*d)
  = {factoring out 2*}
    a + 2*(b + 2*c + 4*d)
  = {factoring out 2*}
    a + 2*(b + 2*(c + 2*d))
  = {re-expressing d}
    a + 2*(b + 2*(c + 2*(d + 2*0)))
-}
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

{- Transform an integer into a binary number:
To accomplish this transformation, we can repeatedly
divide by two and taking the remainder until the integer
becomes 0. For example:
  13 `div` 2 = 6 (rem: 1)
   6 `div` 2 = 3 (rem: 0)
   3 `div` 2 = 1 (rem: 1)
   1 `div` 2 = 0 (rem: 1)
  -----------------------
                    1011
-}
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

{- Ensure that all our binary numbers have the same length (8 bits):
The following function truncates or extends a binary number as
appropiate.

Note that the repeat function produces an infinite list, but due to
lazy evaluation, it will only produce as many elements as required.
-}
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- * Transmission

{- Transform a string into a list of bits:
We convert each Char into a Unicode number, to which then
we transform into a byte. Each of the list of bits have to
be concatenated into one list.
-}
encode :: String -> [Bit]
encode = concat.map(make8.int2bin.ord)

{- The following function chops a list of bits up into eight-bit
binary numbers
-}
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- Transform a list of bits into the corresponding string
decode :: [Bit] -> String
decode = map(chr.bin2int).chop8

{- Simulates the transmission of a string of characters as a list of
bits, using a perfect communication channel that we
model using the identity function.
-}
transmit :: String -> String
transmit = decode.channel.encode

channel :: [Bit] -> [Bit]
channel = id
