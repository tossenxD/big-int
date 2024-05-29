module BandwidthConverter where

-- Simple Haskell module to compute bandwidth metric for addition

type Runtimes   = (Float,Float,Float,Float,Float,Float,Float,Float,Float,Float)
type Bandwidths = (Float,Float,Float,Float,Float,Float,Float,Float,Float,Float)

bandwidth :: Float -> Float -> Float -> Float -> Float
bandwidth b m n t = (3 * n * m * (b / 8)) / (1000 * t)

bandwidthI :: Float -> Runtimes -> Bandwidths
bandwidthI b (t0,t1,t2,t3,t4,t5,t6,t7,t8,t9) =
  (comp 18 14 t0, comp 17 15 t1, comp 16 16 t2, comp 15 17 t3, comp 14 18 t4,
   comp 13 19 t5, comp 12 20 t6, comp 11 21 t7, comp 10 22 t8, comp 9  23 t9)
  where comp e0 e1 t = bandwidth b ((2^e0) / b) (2^e1) t

bandwidthU64 :: Runtimes -> Bandwidths
bandwidthU64 = bandwidthI 64

bandwidthU32 :: Runtimes -> Bandwidths
bandwidthU32 = bandwidthI 32
