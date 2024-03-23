import "helper"
import "add"

--------------------------------------------------------------------------------
-- helper functions
--------------------------------------------------------------------------------

def iterate32 (l: u32, h: u32, c: u32) (a: u32) (b: u32) : (u32, u32, u32) =
  -- compute the low and high part of result
  let r  = (u64.u32 a) * (u64.u32 b)
  let lr = u32.u64 r
  let hr = u32.u64 (r >> 32)
  -- update l, h and c
  let ln = l + lr
  let hn = h + hr + (u32.bool (ln < lr))
  let cn = c + (u32.bool (hn < h))
  in (ln, hn, cn)

def iterate64 (l: u64, h: u64, c: u64) (a: u64) (b: u64) : (u64, u64, u64) =
  -- compute the low and high part of result
  let lr  = a * b
  let hr = u64.mul_hi a b
  -- update l, h and c
  let ln = l + lr
  let hn = h + hr + (u64.bool (ln < lr))
  let cn = c + (u64.bool (hn < h))
  in (ln, hn, cn)


--------------------------------------------------------------------------------
-- multiplication by convolution ( O(n^2) )
--------------------------------------------------------------------------------

def convMult32 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  -- function that computes a low, high and carry part of multiplication
  let convMultLhcs [n] (as: [n]u32) (bs: [n]u32) (tid: i64) :
    ( (u32, u32, u32), (u32, u32, u32) ) =
    let k1 = tid
    let k2 = n-1 - k1
    let lhc1 = loop lhc = (0u32,0u32,0u32) for i < k1 do
               let j = k1 - i in iterate32 lhc as[i] bs[j]
    let lhc2 = loop lhc = (0u32,0u32,0u32) for i < k2 do
               let j = k2 - i in iterate32 lhc as[i] bs[j]
    in (lhc1, lhc2)
  -- find two low-, high- and carry-parts for each thread
  let (lhcs1, lhcs2) = map (convMultLhcs as bs) (iota (n/2)) |> unzip
  let (ls1, hs1, cs1) = unzip3 lhcs1
  let (ls2, hs2, cs2) = unzip3 lhcs2
  let ls = ls1 ++ ls2 :> [n]u32
  let hs = hs1 ++ hs2
  let hs = map (\ i -> if i == 0 then 0 else hs[i-1]) (iota n)
  let cs = cs1 ++ cs2
  let cs = map (\ i -> if i <= 1 then 0 else cs[i-2]) (iota n)
  -- add the low, high and carry parts
  in badd32v1 ls hs |> badd32v1 cs

def convMult64 [n] (as: [n]u64) (bs: [n]u64) : [n]u64 =
  -- function that computes a low, high and carry part of multiplication
  let convMultLhcs [n] (as: [n]u64) (bs: [n]u64) (tid: i64) :
    ( (u64, u64, u64), (u64, u64, u64) ) =
    let k1 = tid
    let k2 = n-1 - k1
    let lhc1 = loop lhc = (0u64,0u64,0u64) for i < k1 do
               let j = k1 - i in iterate64 lhc as[i] bs[j]
    let lhc2 = loop lhc = (0u64,0u64,0u64) for i < k2 do
               let j = k2 - i in iterate64 lhc as[i] bs[j]
    in (lhc1, lhc2)
  -- find two low-, high- and carry-parts for each thread
  let (lhcs1, lhcs2) = map (convMultLhcs as bs) (iota (n/2)) |> unzip
  let (ls1, hs1, cs1) = unzip3 lhcs1
  let (ls2, hs2, cs2) = unzip3 lhcs2
  let ls = ls1 ++ ls2 :> [n]u64
  let hs = hs1 ++ hs2
  let hs = map (\ i -> if i == 0 then 0 else hs[i-1]) (iota n)
  let cs = cs1 ++ cs2
  let cs = map (\ i -> if i <= 1 then 0 else cs[i-2]) (iota n)
  -- add the low, high and carry parts
  in badd64v1 ls hs |> badd64v1 cs
