import "helper"
import "add"

--------------------------------------------------------------------------------
-- helper functions
--------------------------------------------------------------------------------

def iterate32 (l: u32, h: u32, c: u32) (a: u32) (b: u32) : (u32, u32, u32) =
  -- compute the low and high part of result
  let lr = a * b
  let hr = u32.mul_hi a b
  -- update l, h and c
  let ln = l + lr
  let hn = h + hr + (u32.bool (ln < l))
  let cn = c + (u32.bool (hn < h))
  in (ln, hn, cn)

def iterate64 (l: u64, h: u64, c: u64) (a: u64) (b: u64) : (u64, u64, u64) =
  -- compute the low and high part of result
  let lr = a * b
  let hr = u64.mul_hi a b
  -- update l, h and c
  let ln = l + lr
  let hn = h + hr + (u64.bool (ln < l))
  let cn = c + (u64.bool (hn < h))
  in (ln, hn, cn)

def combine64 (l0:u64,h0:u64,c0:u64) (l1:u64,h1:u64,c1:u64): (u64,u64,u64,u64) =
  let h0l1 = h0 + l1
  let h1c0c = h1 + c0 + (u64.bool (h0l1 < h0))
  let c1c = c1 + (u64.bool (h1c0c < h1))
  in (l0, h0l1, h1c0c, c1c)


--------------------------------------------------------------------------------
-- multiplication by convolution ( O(n^2) ) with two elements per thread
--------------------------------------------------------------------------------

def convMult32v1 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  -- function that computes a low, high and carry part of multiplication
  let convMultLhcs (tid: i64) : ( (u32, u32, u32), (u32, u32, u32) ) =
    let k1 = tid
    let k2 = n-1 - k1
    let lhc1 = loop lhc = (0u32,0u32,0u32) for i < k1 + 1 do
               let j = k1 - i in iterate32 lhc as[i] bs[j]
    let lhc2 = loop lhc = (0u32,0u32,0u32) for i < k2 + 1 do
               let j = k2 - i in iterate32 lhc as[i] bs[j]
    in (lhc1, lhc2)
  -- find two low-, high- and carry-parts for each thread
  let (lhcs1, lhcs2) = map convMultLhcs (0..<n/2) |> unzip
  let (ls1, hs1, cs1) = unzip3 lhcs1
  let (ls2, hs2, cs2) = unzip3 <| reverse lhcs2
  let ls = ls1 ++ ls2 :> [n]u32
  let hs = hs1 ++ hs2
  let hs = map (\ i -> if i == 0 then 0 else hs[i-1]) (iota n)
  let cs = cs1 ++ cs2
  let cs = map (\ i -> if i <= 1 then 0 else cs[i-2]) (iota n)
  -- add the low, high and carry parts
  in badd32v1 ls hs |> badd32v1 cs

def convMult64v1 [n] (as: [n]u64) (bs: [n]u64) : [n]u64 =
  -- function that computes a low, high and carry part of multiplication
  let convMultLhcs (tid: i64) : ( (u64, u64, u64), (u64, u64, u64) ) =
    let k1 = tid
    let k2 = n-1 - k1
    let lhc1 = loop lhc = (0u64,0u64,0u64) for i < k1 + 1 do
               let j = k1 - i in iterate64 lhc as[i] bs[j]
    let lhc2 = loop lhc = (0u64,0u64,0u64) for i < k2 + 1 do
               let j = k2 - i in iterate64 lhc as[i] bs[j]
    in (lhc1, lhc2)
  -- find two low-, high- and carry-parts for each thread
  let (lhcs1, lhcs2) = map convMultLhcs (iota (n/2)) |> unzip
  let (ls1, hs1, cs1) = unzip3 lhcs1
  let (ls2, hs2, cs2) = unzip3 <| reverse lhcs2
  let ls = ls1 ++ ls2 :> [n]u64
  let hs = hs1 ++ hs2
  let hs = map (\ i -> if i == 0 then 0 else hs[i-1]) (iota n)
  let cs = cs1 ++ cs2
  let cs = map (\ i -> if i <= 1 then 0 else cs[i-2]) (iota n)
  -- add the low, high and carry parts
  in badd64v1 ls hs |> badd64v1 cs

--------------------------------------------------------------------------------
-- + optimized with four elements per thread
--------------------------------------------------------------------------------

def convMult64v2 [m] (as: [m]u64) (bs: [m]u64) : [m]u64 =
  -- function that computes a low, high and carry part of multiplication
  let convMultLhcs (tid: i64) : ( (u64, u64, u64, u64), (u64, u64, u64, u64) ) =
    let k1 = tid * 2
    let (lhc1, lhc2) =
      loop (lhc1,lhc2) = ((0u64,0u64,0u64),(0u64,0u64,0u64)) for i < k1 + 1 do
      let j = k1 - i
      let a = as[i]
      let lhc1 = iterate64 lhc1 a bs[j]
      let lhc2 = iterate64 lhc2 a bs[j+1]
      in (lhc1, lhc2)
    let lhc2 = iterate64 lhc2 as[k1+1] bs[0]

    let k2 = m-1 - k1
    let (lhc3, lhc4) =
      loop (lhc3,lhc4) = ((0u64,0u64,0u64),(0u64,0u64,0u64)) for i < k2 do
      let j = k2-1 - i
      let a = as[i]
      let lhc3 = iterate64 lhc3 a bs[j]
      let lhc4 = iterate64 lhc4 a bs[j+1]
      in (lhc3, lhc4)
    let lhc4 = iterate64 lhc4 as[k2] bs[0]

    in (combine64 lhc1 lhc2, combine64 lhc3 lhc4)

  -- find two low-, high- and carry-parts for each thread
  let (lhcs1, lhcs2) = map convMultLhcs (iota (m/4)) |> unzip
  let lhcss = lhcs1 ++ (reverse lhcs2)
  let (lhcss1, lhcss2) = map (\i -> (lhcss[i*2],lhcss[i*2+1])) (0..<m/4) |>unzip
  let (l1s, hl1s, ch1s, cc1s) = unzip4 lhcss1
  let (l2s, hl2s, ch2s, cc2s) = unzip4 lhcss2
  let lhcs1sh = l1s ++ hl1s ++ ch1s ++ cc1s :> [m]u64
  let lhcs2sh = l2s ++ hl2s ++ ch2s ++ cc2s :> [m]u64
  let lhcs2sh = map (\ i -> if i <= 1 then 0 else lhcs2sh[i-2]) (iota m)
  in badd64v2 lhcs1sh lhcs2sh

--------------------------------------------------------------------------------
-- + optimized by allowing multiple instances per block
--------------------------------------------------------------------------------

-- TODO

def convMult64v3 [m] (ipb: i64) (as: [m]u64) (bs: [m]u64) : [m]u64 =
  -- size of each instance
  let n = m / ipb

  -- function that computes a low, high and carry part of multiplication
  let convMultLhcs (tid: i64) : ( (u64, u64, u64, u64), (u64, u64, u64, u64) ) =
    let k1 = tid * 2
    let k1_start = (k1 / n) * n
    let (lhc1, lhc2) =
      loop (lhc1,lhc2) = ( (0u64, 0u64, 0u64), (0u64, 0u64, 0u64) )
      for i < (k1 + 1 - k1_start) do
      let j = k1 - i
      let a = as[i + k1_start]
      let lhc1 = iterate64 lhc1 a bs[j]
      let lhc2 = iterate64 lhc2 a bs[j+1]
      in (lhc1, lhc2)
    let lhc2 = iterate64 lhc2 as[k1+1] bs[k1_start]

    let k2 = m-1 - k1
    let k2_start = (k2 / n) * n
    let (lhc3, lhc4) =
      loop (lhc3,lhc4) = ( (0u64, 0u64, 0u64), (0u64, 0u64, 0u64) )
      for i < (k2 - k2_start) do
      let j = k2-1 - i
      let a = as[i + k2_start]
      let lhc3 = iterate64 lhc3 a bs[j]
      let lhc4 = iterate64 lhc4 a bs[j+1]
      in (lhc3, lhc4)
    let lhc4 = iterate64 lhc4 as[k2] bs[k2_start]

    in (combine64 lhc1 lhc2, combine64 lhc3 lhc4)

  -- find two low-, high- and carry-parts for each thread
  let (lhcs1, lhcs2) = map convMultLhcs (iota (m/4)) |> unzip
  let lhcss = lhcs1 ++ (reverse lhcs2)
  let (lhcss1, lhcss2) = map (\i -> (lhcss[i*2],lhcss[i*2+1])) (0..<m/4) |>unzip
  let (l1s, hl1s, ch1s, cc1s) = unzip4 lhcss1
  let (l2s, hl2s, ch2s, cc2s) = unzip4 lhcss2
  let lhcs1sh = l1s ++ hl1s ++ ch1s ++ cc1s :> [m]u64
  let lhcs2sh = l2s ++ hl2s ++ ch2s ++ cc2s
  let lhcs2sh = map (\ i -> if i <= 1 then 0 else lhcs2sh[i-2]) (iota m)
  in badd64v3 ipb lhcs1sh lhcs2sh -- TODO since its already in shared, should call RUN version

entry convMult64v3Wrapper [n][m] (ass: [n][m]u64) (bss: [n][m]u64) : [n][m]u64 =
  let ipb = if m > 4 then (128 + (m/4) - 1) / (m/4) else 1 -- ceil(128/(m/4))
  let ipb = if ipb > n || (n % ipb) > 0 then 1 else ipb
  -- let ipb = 2
  let as  = (flatten ass :> [(n/ipb)*(ipb*m)]u64) |> unflatten
  let bs  = (flatten bss :> [(n/ipb)*(ipb*m)]u64) |> unflatten
  let rs  = imap2Intra as bs (convMult64v3 ipb)
  in (flatten rs :> [n*m]u64) |> unflatten
