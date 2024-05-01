module Debug where

import Data.Bits

b :: Integer
b = 32

fromList :: [Integer] -> Integer
fromList = foldl (\acc (i, x) -> acc + ((2^b)^i) * x) 0 . zip [0..]

toList :: Integer -> [Integer]
toList u | u == 0 = []
         | otherwise = ((.&.) u ((2^b)-1)) : toList (shiftR u (fromIntegral b))


powdiff :: Integer -> Integer -> Integer -> Integer
powdiff h v w = ((2^b)^h) - (v * w)


step1 :: Integer -> Int -> Integer
step1 w n = shift w (n * (fromIntegral b))

step2 :: Int -> Integer -> Integer -> Int -> Integer
step2 h v w n =
  shift (w * (powdiff (fromIntegral (h - n)) v w)) ((2*n - h) * (fromIntegral b))

step :: Int -> Integer -> Integer -> Int -> Integer
step h v w n =
  (step1 w n) + (step2 h v w n)


whileloop :: Integer -> Int -> Int -> Integer -> Int -> Int -> Integer
whileloop v h k w l g
  | h - k <= l = w
  | otherwise =
    let n = min (h - k + 1 - l) l
        s = max 0 (k - 2*l + 1 - g)
        wn = shift (step (k+l+n-s+g) (shift v (-s)) w n) (-1)
        ln = l + n - 1
    in whileloop v h k wn ln g

refine3 :: Integer -> Int -> Int -> Integer -> Int -> Integer
refine3 v h k w l =
  let g  = 2
      wg = shift w (g * (fromIntegral b))
      r  = whileloop v h k wg l g
  in shift r ((-g) * (fromIntegral b))