module Gu32opsConverter where

-- Simple Haskell module to compute Gu32ops metric for multiplication

type Runtimes = (Float,Float,Float,Float,Float,Float,Float,Float,Float,Float)
type Gu32opss = (Float,Float,Float,Float,Float,Float,Float,Float,Float,Float)

gu32ops :: Float -> Float -> Float -> Float -> Float
gu32ops b m n t = (n * 300 * mU32 * (logBase 2 mU32)) / (1000 * t)
  where mU32 = m * (b / 32)

gu32opsI :: Float -> Float -> Runtimes -> Gu32opss
gu32opsI b c (t0,t1,t2,t3,t4,t5,t6,t7,t8,t9) =
  (comp 18 14 t0, comp 17 15 t1, comp 16 16 t2, comp 15 17 t3, comp 14 18 t4,
   comp 13 19 t5, comp 12 20 t6, comp 11 21 t7, comp 10 22 t8, comp 9  23 t9)
  where comp e0 e1 t = c * (gu32ops b ((2^e0) / b) (2^e1) t)

gu32opsU64one :: Runtimes -> Gu32opss
gu32opsU64one = gu32opsI 64 1

gu32opsU64six :: Runtimes -> Gu32opss
gu32opsU64six = gu32opsI 64 6

gu32opsU32one :: Runtimes -> Gu32opss
gu32opsU32one = gu32opsI 32 1

gu32opsU32six :: Runtimes -> Gu32opss
gu32opsU32six = gu32opsI 32 6
