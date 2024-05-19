import "helper"
import "add"

--------------------------------------------------------------------------------
-- helper functions
--------------------------------------------------------------------------------

def iterate (l: ui, h: ui, c: ui) (a: ui) (b: ui) : (ui, ui, ui) =
  -- compute the low and high part of result
  let lr = a * b
  let hr = mulHigh a b
  -- update l, h and c
  let ln = l + lr
  let hn = h + hr + (fromBool (ln < l))
  let cn = c + (fromBool (hn < h))
  in (ln, hn, cn)

def combine (l0: ui, h0: ui, c0: ui) (l1: ui, h1: ui, c1: ui): (ui, ui, ui, ui)=
  let h0l1 = h0 + l1
  let h1c0c = h1 + c0 + (fromBool (h0l1 < h0))
  let c1c = c1 + (fromBool (h1c0c < h1))
  in (l0, h0l1, h1c0c, c1c)


--------------------------------------------------------------------------------
-- multiplication by convolution ( O(n^2) ) with two elements per thread
--------------------------------------------------------------------------------

def convMultV1 [m] (as: [2*m]ui) (bs: [2*m]ui) : [2*m]ui =
  -- function that computes a low, high and carry part of multiplication
  let convMultLhcs (ash: []ui) (bsh: []ui) (tid: i64)
      : ( (ui, ui, ui), (ui, ui, ui) ) =
    let k1 = tid
    let lhc1 : (ui, ui, ui) = loop lhc = (0, 0, 0) for i < k1 + 1 do
                              let j = k1 - i in iterate lhc ash[i] bsh[j]
    let k2 = 2*m-1 - k1
    let lhc2 : (ui, ui, ui) = loop lhc = (0, 0, 0) for i < k2 + 1 do
                              let j = k2 - i in iterate lhc ash[i] bsh[j]
    in (lhc1, lhc2)

  -- copy to shared memory TODO
  let ash = as
  let bsh = bs

  -- find two low-, high- and carry-parts for each thread
  let (lhcs1, lhcs2) = map (convMultLhcs ash bsh) (0..<m) |> unzip
  let (ls1, hs1, cs1) = unzip3 lhcs1
  let (ls2, hs2, cs2) = unzip3 <| reverse lhcs2
  let ls = ls1 ++ ls2 :> [2*m]ui
  let hs = hs1 ++ hs2
  let hs = map (\ i -> if i == 0 then 0 else hs[i-1]) (0..<2*m)
  let cs = cs1 ++ cs2
  let cs = map (\ i -> if i <= 1 then 0 else cs[i-2]) (0..<2*m)
  -- add the low, high and carry parts
  in baddV1 ls hs |> baddV1 cs


--------------------------------------------------------------------------------
-- + optimized with four elements per thread
--------------------------------------------------------------------------------

def convMultV2 [m] (as: [4*m]ui) (bs: [4*m]ui) : [4*m]ui =
  -- function that computes a low, high and carry part of multiplication
  let convMultLhcs (ash: []ui) (bsh: []ui) (tid: i64)
      : ( (ui, ui, ui, ui), (ui, ui, ui, ui) ) =
    let k1 = tid * 2
    let (lhc1, lhc2) : ( (ui, ui, ui), (ui, ui, ui) ) =
      loop (lhc1,lhc2) = ( (0, 0, 0), (0, 0, 0) ) for i < k1 + 1 do
      let j = k1 - i
      let a = ash[i]
      let lhc1 = iterate lhc1 a bsh[j]
      let lhc2 = iterate lhc2 a bsh[j+1]
      in (lhc1, lhc2)
    let lhc2 = iterate lhc2 ash[k1+1] bsh[0]

    let k2 = m*4 - 1 - k1
    let (lhc3, lhc4) : ( (ui, ui, ui), (ui, ui, ui) ) =
      loop (lhc3,lhc4) = ( (0, 0, 0), (0, 0, 0) ) for i < k2 do
      let j = k2-1 - i
      let a = ash[i]
      let lhc3 = iterate lhc3 a bsh[j]
      let lhc4 = iterate lhc4 a bsh[j+1]
      in (lhc3, lhc4)
    let lhc4 = iterate lhc4 ash[k2] bsh[0]

    in (combine lhc1 lhc2, combine lhc3 lhc4)

  -- copy to shared memory TODO
  let ash = as
  let bsh = bs

  -- find two low-, high- and carry-parts for each thread
  let (lhcs1, lhcs2) = map (convMultLhcs ash bsh) (0..<m) |> unzip
  let lhcs = lhcs1 ++ (reverse lhcs2)

  -- map the convolution result to memory
  let (ls, hls, chs, ccs) = unzip4 lhcs
  let lhcss = ls ++ hls ++ chs ++ ccs :> [8*m]ui

  -- compute indices and retrieve convolution result from memory
  let (inds1, inds2) =
    unzip <| imap (0..<2*m) (\ i -> let off = i * 2
                                    let inds = (off, off+1, off+2, off+3)
                                    let disc = (-1, -1, -1, -1)
                                    in if bool.i64 (i % 2) -- if odd
                                       then ( inds, disc )
                                       else ( disc, inds ))

  let (inds11, inds12, inds13, inds14) = unzip4 inds1
  let indss1 = inds11 ++ inds12 ++ inds13 ++ inds14 :> [8*m]i64

  let (inds21, inds22, inds23, inds24) = unzip4 inds2
  let indss2 = inds21 ++ inds22 ++ inds23 ++ inds24 :> [8*m]i64

  let lhcss1 = scatter (replicate (4*m) 0) indss1 lhcss
  let lhcss2 = scatter (replicate (4*m) 0) indss2 lhcss

  -- add the convolution parts
  in baddV2 lhcss1 lhcss2


--------------------------------------------------------------------------------
-- + optimized by allowing multiple instances per block
--------------------------------------------------------------------------------

def convMultV3 [ipb][m] (as: [ipb*(4*m)]ui) (bs: [ipb*(4*m)]ui) : [ipb*(4*m)]ui=
  -- function that computes a low, high and carry part of multiplication
  let convMultLhcs (ash: []ui) (bsh: []ui) (tid: i64)
      : ( (ui, ui, ui, ui), (ui, ui, ui, ui) ) =
    let k1 = tid * 2
    let k1_start = (k1 / (4*m)) * (4*m)
    let (lhc1, lhc2) : ( (ui, ui, ui), (ui, ui, ui) ) =
      loop (lhc1,lhc2) = ( (0, 0, 0), (0, 0, 0) ) for i < (k1 + 1 - k1_start) do
      let j = k1 - i
      let a = ash[i + k1_start]
      let lhc1 = iterate lhc1 a bsh[j]
      let lhc2 = iterate lhc2 a bsh[j+1]
      in (lhc1, lhc2)
    let lhc2 = iterate lhc2 ash[k1+1] bsh[k1_start]

    let k2 = ipb*4*m - 1 - k1
    let k2_start = (k2 / (4*m)) * (4*m)
    let (lhc3, lhc4) : ( (ui, ui, ui), (ui, ui, ui) ) =
      loop (lhc3,lhc4) = ( (0, 0, 0), (0, 0, 0) ) for i < (k2 - k2_start) do
      let j = k2-1 - i
      let a = ash[i + k2_start]
      let lhc3 = iterate lhc3 a bsh[j]
      let lhc4 = iterate lhc4 a bsh[j+1]
      in (lhc3, lhc4)
    let lhc4 = iterate lhc4 ash[k2] bsh[k2_start]

    in (combine lhc1 lhc2, combine lhc3 lhc4)

  -- copy to shared memory TODO
  let ash = as
  let bsh = bs

  -- find two low-, high- and carry-parts for each thread
  let (lhcs1, lhcs2) = map (convMultLhcs ash bsh) (0..<ipb*m) |> unzip
  let lhcs = lhcs1 ++ (reverse lhcs2)

  -- map the convolution result to memory
  let (ls, hls, chs, ccs) = unzip4 lhcs
  let lhcss = ls ++ hls ++ chs ++ ccs :> [8*ipb*m]ui

  -- compute indices and retrieve convolution result from memory
  let (inds1, inds2) =
    unzip <| imap (0..<2*ipb*m) (\ i -> let off = i * 2
                                        let isOdd = bool.i64 (i % 2)
                                        let inds = if isOdd && ((off + 2) % (4*m) == 0)
                                                   then (off, off+1, -1, -1)
                                                   else (off, off+1, off+2, off+3)
                                        let disc = (-1, -1, -1, -1)
                                        in if isOdd
                                           then ( inds, disc )
                                           else ( disc, inds ))

  let (inds11, inds12, inds13, inds14) = unzip4 inds1
  let indss1 = inds11 ++ inds12 ++ inds13 ++ inds14 :> [8*ipb*m]i64

  let (inds21, inds22, inds23, inds24) = unzip4 inds2
  let indss2 = inds21 ++ inds22 ++ inds23 ++ inds24 :> [8*ipb*m]i64

  let lhcss1 = scatter (replicate (ipb*(4*m)) 0) indss1 lhcss
  let lhcss2 = scatter (replicate (ipb*(4*m)) 0) indss2 lhcss

  -- add the convolution parts
  in baddV3 lhcss1 lhcss2


--------------------------------------------------------------------------------
-- Callers
--------------------------------------------------------------------------------

-- callers for one multiplication

entry oneMulV1 [n] (m: i64) (uss: [n][2*m]ui) (vss: [n][2*m]ui) : [n][2*m]ui =
  imap2Intra uss vss convMultV1

entry oneMulV2 [n] (m: i64) (uss: [n][4*m]ui) (vss: [n][4*m]ui) : [n][4*m]ui =
  imap2Intra uss vss convMultV2

entry oneMulV3 [n][ipb] (m: i64) (usss: [n][ipb][4*m]ui) (vsss: [n][ipb][4*m]ui) : [n][ipb][4*m]ui =
  let uss = map flatten usss :> [n][ipb*(4*m)]ui
  let vss = map flatten vsss :> [n][ipb*(4*m)]ui
  let wss = imap2Intra uss vss convMultV3
  in map unflatten wss

-- callers for six multiplications, computing `a^6 * b` for input `a` and `b`

entry sixMulV1 [n] (m: i64) (uss: [n][2*m]ui) (vss: [n][2*m]ui) : [n][2*m]ui =
  let wss = imap2Intra uss vss convMultV1
  let wss = imap2Intra uss wss convMultV1
  let wss = imap2Intra uss wss convMultV1
  let wss = imap2Intra uss wss convMultV1
  let wss = imap2Intra uss wss convMultV1
  in imap2Intra uss wss convMultV1

entry sixMulV2 [n] (m: i64) (uss: [n][4*m]ui) (vss: [n][4*m]ui) : [n][4*m]ui =
  let wss = imap2Intra uss vss convMultV2
  let wss = imap2Intra uss wss convMultV2
  let wss = imap2Intra uss wss convMultV2
  let wss = imap2Intra uss wss convMultV2
  let wss = imap2Intra uss wss convMultV2
  in imap2Intra uss wss convMultV2

entry sixMulV3 [n][ipb] (m: i64) (usss: [n][ipb][4*m]ui) (vsss: [n][ipb][4*m]ui) : [n][ipb][4*m]ui =
  let uss = map flatten usss :> [n][ipb*(4*m)]ui
  let vss = map flatten vsss :> [n][ipb*(4*m)]ui
  let wss = imap2Intra uss vss convMultV3
  let wss = imap2Intra uss wss convMultV3
  let wss = imap2Intra uss wss convMultV3
  let wss = imap2Intra uss wss convMultV3
  let wss = imap2Intra uss wss convMultV3
  let wss = imap2Intra uss wss convMultV3
  in map unflatten wss
