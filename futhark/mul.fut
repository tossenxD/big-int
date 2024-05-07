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
-- if size is uneven, the last thread stays idle for upper computation

def convMult32v1 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  -- function that computes a low, high and carry part of multiplication
  let convMultLhcs (ash: []u32) (bsh: []u32) (tid: i64)
      : ( (u32, u32, u32), (u32, u32, u32) ) =
    let k1 = tid
    let k2 = n-1 - k1
    let lhc1 = loop lhc = (0u32,0u32,0u32) for i < k1 + 1 do
               let j = k1 - i in iterate32 lhc ash[i] bsh[j]
    let lhc2 =
      if tid == n/2 then (0u32,0u32,0u32)
      else loop lhc = (0u32,0u32,0u32) for i < k2 + 1 do
           let j = k2 - i in iterate32 lhc ash[i] bsh[j]
    in (lhc1, lhc2)

  -- copy to shared memory TODO
  let ash = as
  let bsh = bs

  -- find two low-, high- and carry-parts for each thread
  let (lhcs1, lhcs2) = map (convMultLhcs ash bsh) (0..<(n+1)/2) |> unzip
  let (ls1, hs1, cs1) = unzip3 lhcs1
  let (ls2, hs2, cs2) = unzip3 <| reverse <| take (n/2) lhcs2
  let ls = ls1 ++ ls2 :> [n]u32
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
    let lhc2 =
      if tid == n/2 then (0u64,0u64,0u64)
      else loop lhc = (0u64,0u64,0u64) for i < k2 + 1 do
           let j = k2 - i in iterate64 lhc ash[i] bsh[j]
    in (lhc1, lhc2)

  -- copy to shared memory TODO
  let ash = as
  let bsh = bs

  -- find two low-, high- and carry-parts for each thread
  let (lhcs1, lhcs2) = map (convMultLhcs ash bsh) (0..<(n+1)/2) |> unzip
  let (ls1, hs1, cs1) = unzip3 lhcs1
  let (ls2, hs2, cs2) = unzip3 <| reverse <| take (n/2) lhcs2
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
-- the size is padded to be a multiple of 4 if it isn't already

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

  -- copy to shared memory TODO
  let ash = as
  let bsh = bs

  -- find two low-, high- and carry-parts for each thread
  let (lhcs1, lhcs2) = map (convMultLhcs ash bsh) (0..<m/4) |> unzip
  let lhcs = lhcs1 ++ (reverse lhcs2)

  -- map the convolution result to memory
  let (ls, hls, chs, ccs) = unzip4 lhcs
  let lhcss = ls ++ hls ++ chs ++ ccs :> [2*m]u64

  -- compute indices and retrieve convolution result from memory
  let (inds1, inds2) =
    unzip <| imap (0..<m/2) (\ i -> let off = i * 2
                                    let inds = (off, off+1, off+2, off+3)
                                    let disc = (-1, -1, -1, -1)
                                    in if bool.i64 (i % 2) -- if odd
                                       then ( inds, disc )
                                       else ( disc, inds ))

  let (inds11, inds12, inds13, inds14) = unzip4 inds1
  let indss1 = inds11 ++ inds12 ++ inds13 ++ inds14 :> [2*m]i64

  let (inds21, inds22, inds23, inds24) = unzip4 inds2
  let indss2 = inds21 ++ inds22 ++ inds23 ++ inds24 :> [2*m]i64

  let lhcss1 = scatter (replicate m 0) indss1 lhcss
  let lhcss2 = scatter (replicate m 0) indss2 lhcss

  -- add the convolution parts
  in badd64v2 lhcss1 lhcss2

def convMult64v2Safe [m] (as: [m]u64) (bs: [m]u64) : [m]u64 =
  let asPad = pad1d 4 0 as
  let bsPad = pad1d 4 0 bs
  in convMult64v2 asPad bsPad |> take m


--------------------------------------------------------------------------------
-- + optimized by allowing multiple instances per block
--------------------------------------------------------------------------------
-- the size is padded to be a multiple of 4 and `ipb` if it isn't already

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

  -- copy to shared memory TODO
  let ash = as
  let bsh = bs

  -- find two low-, high- and carry-parts for each thread
  let (lhcs1, lhcs2) = map (convMultLhcs ash bsh) (0..<m/4) |> unzip
  let lhcs = lhcs1 ++ (reverse lhcs2)

  -- map the convolution result to memory
  let (ls, hls, chs, ccs) = unzip4 lhcs
  let lhcss = ls ++ hls ++ chs ++ ccs :> [2*m]u64

  -- compute indices and retrieve convolution result from memory
  let (inds1, inds2) =
    unzip <| imap (0..<m/2) (\ i -> let off = i * 2
                                    let isOdd = bool.i64 (i % 2)
                                    let inds = if isOdd && ((off + 2) % n == 0)
                                               then (off, off+1, -1, -1)
                                               else (off, off+1, off+2, off+3)
                                    let disc = (-1, -1, -1, -1)
                                    in if isOdd
                                       then ( inds, disc )
                                       else ( disc, inds ))

  let (inds11, inds12, inds13, inds14) = unzip4 inds1
  let indss1 = inds11 ++ inds12 ++ inds13 ++ inds14 :> [2*m]i64

  let (inds21, inds22, inds23, inds24) = unzip4 inds2
  let indss2 = inds21 ++ inds22 ++ inds23 ++ inds24 :> [2*m]i64

  let lhcss1 = scatter (replicate m 0) indss1 lhcss
  let lhcss2 = scatter (replicate m 0) indss2 lhcss

  -- add the convolution parts
  in badd64v3 ipb lhcss1 lhcss2

entry convMult64v3Wrapper [n][m] (ass: [n][m]u64) (bss: [n][m]u64) : [n][m]u64 =
  let assPad = map (pad1d 4 0) ass
  let bssPad = map (pad1d 4 0) bss
  let mm  = m + (4 - (m % 4)) % 4
  let ipb = (128 + (mm/4) - 1) / (mm/4) -- ceil(128/(mm/4))
  let ipb = if ipb > n || (n % ipb) > 0 then 1 else ipb
  let as  = (flatten assPad :> [(n/ipb)*(ipb*mm)]u64) |> unflatten
  let bs  = (flatten bssPad :> [(n/ipb)*(ipb*mm)]u64) |> unflatten
  let rs  = imap2Intra as bs (convMult64v3 ipb)
  in (flatten rs :> [n*mm]u64) |> unflatten |> map (take m)

-- Wrapper for 1d arrays; written for testing, use v2 instead
entry convMult64v3Safe1d [m] (as: [m]u64) (bs: [m]u64) : [m]u64 =
  convMult64v3 1 (pad1d 4 0 as) (pad1d 4 0 bs) |> take m
