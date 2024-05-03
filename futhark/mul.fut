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
  let convMultLhcs (ash: []u32) (bsh: []u32) (tid: i64)
      : ( (u32, u32, u32), (u32, u32, u32) ) =
    let k1 = tid
    let k2 = n-1 - k1
    let lhc1 = loop lhc = (0u32,0u32,0u32) for i < k1 + 1 do
               let j = k1 - i in iterate32 lhc ash[i] bsh[j]
    let lhc2 = loop lhc = (0u32,0u32,0u32) for i < k2 + 1 do
               let j = k2 - i in iterate32 lhc ash[i] bsh[j]
    in (lhc1, lhc2)

  -- add padding
  let p = n % 2
  let pz = replicate p 0
  let ash = as ++ pz
  let bsh = bs ++ pz

  -- find two low-, high- and carry-parts for each thread
  let (lhcs1, lhcs2) = map (convMultLhcs ash bsh) (0..<(n+1)/2) |> unzip
  let (ls1, hs1, cs1) = unzip3 lhcs1
  let (ls2, hs2, cs2) = unzip3 <| reverse lhcs2
  let ls = ls1 ++ ls2 |> take n
  let hs = hs1 ++ hs2
  let hs = map (\ i -> if i == 0 then 0 else hs[i-1]) (iota n)
  let cs = cs1 ++ cs2
  let cs = map (\ i -> if i <= 1 then 0 else cs[i-2]) (iota n)
  -- add the low, high and carry parts
  in badd32v1 ls hs |> badd32v1 cs

def convMult64v1 [n] (as: [n]u64) (bs: [n]u64) : [n]u64 =
  -- function that computes a low, high and carry part of multiplication
  let convMultLhcs (ash: []u64) (bsh: []u64) (tid: i64)
      : ( (u64, u64, u64), (u64, u64, u64) ) =
    let k1 = tid
    let k2 = n-1 - k1
    let lhc1 = loop lhc = (0u64,0u64,0u64) for i < k1 + 1 do
               let j = k1 - i in iterate64 lhc ash[i] bsh[j]
    let lhc2 = loop lhc = (0u64,0u64,0u64) for i < k2 + 1 do
               let j = k2 - i in iterate64 lhc ash[i] bsh[j]
    in (lhc1, lhc2)

  -- add padding
  let p = n % 2
  let pz = replicate p 0
  let ash = as ++ pz
  let bsh = bs ++ pz

  -- find two low-, high- and carry-parts for each thread
  let (lhcs1, lhcs2) = map (convMultLhcs ash bsh) (iota ((n+1)/2)) |> unzip
  let (ls1, hs1, cs1) = unzip3 lhcs1
  let (ls2, hs2, cs2) = unzip3 <| reverse lhcs2
  let ls = ls1 ++ ls2 |> take n
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
  let convMultLhcs (ash: []u64) (bsh: []u64) (tid: i64)
      : ( (u64, u64, u64, u64), (u64, u64, u64, u64) ) =
    let k1 = tid * 2
    let (lhc1, lhc2) =
      loop (lhc1,lhc2) = ((0u64,0u64,0u64),(0u64,0u64,0u64)) for i < k1 + 1 do
      let j = k1 - i
      let a = ash[i]
      let lhc1 = iterate64 lhc1 a bsh[j]
      let lhc2 = iterate64 lhc2 a bsh[j+1]
      in (lhc1, lhc2)
    let lhc2 = iterate64 lhc2 ash[k1+1] bsh[0]

    let k2 = m-1 - k1
    let (lhc3, lhc4) =
      loop (lhc3,lhc4) = ((0u64,0u64,0u64),(0u64,0u64,0u64)) for i < k2 do
      let j = k2-1 - i
      let a = ash[i]
      let lhc3 = iterate64 lhc3 a bsh[j]
      let lhc4 = iterate64 lhc4 a bsh[j+1]
      in (lhc3, lhc4)
    let lhc4 = iterate64 lhc4 ash[k2] bsh[0]

    in (combine64 lhc1 lhc2, combine64 lhc3 lhc4)

  -- add padding
  let p = (4 - (m % 4)) % 4
  let pz = replicate p 0
  let ash = as ++ pz
  let bsh = bs ++ pz

  -- find two low-, high- and carry-parts for each thread
  let (lhcs1, lhcs2) = map (convMultLhcs ash bsh) (iota ((m+p)/4)) |> unzip
  let lhcss = lhcs1 ++ (reverse lhcs2)
  let (lhcss1, lhcss2) =
    map (\i -> (lhcss[i*2],lhcss[i*2+1])) (0..<(m+p)/4) |> unzip
  let (l1s, hl1s, ch1s, cc1s) = unzip4 lhcss1
  let (l2s, hl2s, ch2s, cc2s) = unzip4 lhcss2
  let lhcs1sh = l1s ++ hl1s ++ ch1s ++ cc1s |> take m
  let lhcs2sh = l2s ++ hl2s ++ ch2s ++ cc2s
  let lhcs2sh = map (\ i -> if i <= 1 then 0 else lhcs2sh[i-2]) (iota m)
  in badd64v2 lhcs1sh lhcs2sh

--------------------------------------------------------------------------------
-- + optimized by allowing multiple instances per block
--------------------------------------------------------------------------------

def convMult64v3 [m] (ipb: i64) (as: [m]u64) (bs: [m]u64) : [m]u64 =
  -- size of each instance
  let n = m / ipb

  -- function that computes a low, high and carry part of multiplication
  let convMultLhcs (ash: []u64) (bsh: []u64) (tid: i64)
      : ( (u64, u64, u64, u64), (u64, u64, u64, u64) ) =
    let k1 = tid * 2
    let k1_start = (k1 / n) * n
    let (lhc1, lhc2) =
      loop (lhc1,lhc2) = ( (0u64, 0u64, 0u64), (0u64, 0u64, 0u64) )
      for i < (k1 + 1 - k1_start) do
      let j = k1 - i
      let a = ash[i + k1_start]
      let lhc1 = iterate64 lhc1 a bsh[j]
      let lhc2 = iterate64 lhc2 a bsh[j+1]
      in (lhc1, lhc2)
    let lhc2 = iterate64 lhc2 ash[k1+1] bsh[k1_start]

    let k2 = m-1 - k1
    let k2_start = (k2 / n) * n
    let (lhc3, lhc4) =
      loop (lhc3,lhc4) = ( (0u64, 0u64, 0u64), (0u64, 0u64, 0u64) )
      for i < (k2 - k2_start) do
      let j = k2-1 - i
      let a = ash[i + k2_start]
      let lhc3 = iterate64 lhc3 a bsh[j]
      let lhc4 = iterate64 lhc4 a bsh[j+1]
      in (lhc3, lhc4)
    let lhc4 = iterate64 lhc4 ash[k2] bsh[k2_start]

    in (combine64 lhc1 lhc2, combine64 lhc3 lhc4)

  -- add padding
  let p = (4 - (m % 4)) % 4
  let pz = replicate p 0
  let ash = as ++ pz
  let bsh = bs ++ pz

  -- find two low-, high- and carry-parts for each thread
  let (lhcs1, lhcs2) = map (convMultLhcs ash bsh) (iota ((m+p)/4)) |> unzip
  let lhcss = lhcs1 ++ (reverse lhcs2)
  let (lhcss1, lhcss2) =
    map (\i -> (lhcss[i*2],lhcss[i*2+1])) (0..<(m+p)/4) |> unzip
  let (l1s, hl1s, ch1s, cc1s) = unzip4 lhcss1
  let (l2s, hl2s, ch2s, cc2s) = unzip4 lhcss2
  let lhcs1sh = l1s ++ hl1s ++ ch1s ++ cc1s |> take m
  let lhcs2sh = l2s ++ hl2s ++ ch2s ++ cc2s
  let lhcs2sh = map (\ i -> if i <= 1 then 0 else lhcs2sh[i-2]) (iota m)
  in badd64v3 ipb lhcs1sh lhcs2sh

entry convMult64v3Wrapper [n][m] (ass: [n][m]u64) (bss: [n][m]u64) : [n][m]u64 =
  let ipb = if m > 4 then (128 + (m/4) - 1) / (m/4) else 1 -- ceil(128/(m/4))
  let ipb = if ipb > n || (n % ipb) > 0 then 1 else ipb
  let as  = (flatten ass :> [(n/ipb)*(ipb*m)]u64) |> unflatten
  let bs  = (flatten bss :> [(n/ipb)*(ipb*m)]u64) |> unflatten
  let rs  = imap2Intra as bs (convMult64v3 ipb)
  in (flatten rs :> [n*m]u64) |> unflatten
