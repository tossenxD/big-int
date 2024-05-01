module Debug where

import Data.Bits

b64 :: Integer
b64 = 2^64

b32 :: Integer
b32 = 2^32

fromList :: Integer -> [Integer] -> Integer
fromList b = foldl (\acc (i, x) -> acc + ((2^b)^i) * x) 0 . zip [0..]

toList :: Integer -> Integer -> [Integer]
toList b u | u == 0 = []
           | otherwise = ((.&.) u ((2^b)-1)) : toList b (shiftR u (fromIntegral b))
