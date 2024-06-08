import "helper"
import "add"

--------------------------------------------------------------------------------
--- Big Integer Multiplication
--------------------------------------------------------------------------------
--- The implementation is based on the classical O(n^2) algorithm, where the
--- digits of the two integer inputs are convoluted, multiplied, and added.
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

def iterate (l: ui, h: ui, c: ui) (u: ui) (v: ui) : (ui, ui, ui) =
  -- compute the low and high part of result
  let lr = u * v
  let hr = mulHigh u v
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
-- V1: Multiplication by convolution with two elements per thread
--------------------------------------------------------------------------------

def convMulV1 [m] (us: [2*m]ui) (vs: [2*m]ui) : [2*m]ui =
  -- MULTIPLICATION BODY
  let CONV (us: []ui) (vs: []ui) (tid: i64)
      : ( (ui, ui, ui), (ui, ui, ui) ) = #[unsafe]
    let k1 = tid
    let lhc1 : (ui, ui, ui) = loop lhc = (0, 0, 0) for i < k1 + 1 do
                              let j = k1 - i in iterate lhc us[i] vs[j]
    let k2 = 2*m-1 - k1
    let lhc2 : (ui, ui, ui) = loop lhc = (0, 0, 0) for i < k2 + 1 do
                              let j = k2 - i in iterate lhc us[i] vs[j]
    in (lhc1, lhc2)

  -- COPY TO SHARED MEMORY
  let cp2sh (i : i32) = #[unsafe]
    let str = i32.i64 m
    in ( (us[i], us[str + i]), (vs[i], vs[str + i]) )

  -- 1. copy to shared memory coalesced
  let (uss, vss) = (0..<m) |> map i32.i64 |> map cp2sh |> unzip
  let (u1s, u2s) = unzip2 uss
  let (v1s, v2s) = unzip2 vss
  let ush = u1s ++ u2s |> opaque
  let vsh = v1s ++ v2s |> opaque

  -- 2. find two low, high, and carry parts for each thread
  let (lhcs1, lhcs2) = map (CONV ush vsh) (0..<m) |> unzip
  let (ls1, hs1, cs1) = unzip3 lhcs1
  let (ls2, hs2, cs2) = unzip3 <| reverse lhcs2
  let ls = ls1 ++ ls2 :> [2*m]ui
  let hs = hs1 ++ hs2
  let hs = map (\ i -> if i == 0 then 0 else hs[i-1]) (0..<2*m)
  let cs = cs1 ++ cs2
  let cs = map (\ i -> if i <= 1 then 0 else cs[i-2]) (0..<2*m)

  -- 3. add the low, high and carry parts
  in baddV1 ls hs |> baddV1 cs


--------------------------------------------------------------------------------
-- V2: Multiplication by convolution with four elements per thread
--------------------------------------------------------------------------------

def convMulV2 [m] (us: [4*m]ui) (vs: [4*m]ui) : [4*m]ui =
  -- MULTIPLICATION BODY
  let CONV (us: []ui) (vs: []ui) (tid: i64)
      : ( (ui, ui, ui, ui), (ui, ui, ui, ui), (i64, i64, i64, i64) ) = #[unsafe]
    let k1 = tid * 2
    let (lhc1, lhc2) : ( (ui, ui, ui), (ui, ui, ui) ) =
      loop (lhc1,lhc2) = ( (0, 0, 0), (0, 0, 0) ) for i < k1 + 1 do
      let j = k1 - i
      let u = us[i]
      let lhc1 = iterate lhc1 u vs[j]
      let lhc2 = iterate lhc2 u vs[j+1]
      in (lhc1, lhc2)
    let lhc2 = iterate lhc2 us[k1+1] vs[0]
    let (l1, lh1, hc1, cc1) = combine lhc1 lhc2

    let k2 = m*4 - 1 - k1
    let (lhc3, lhc4) : ( (ui, ui, ui), (ui, ui, ui) ) =
      loop (lhc3,lhc4) = ( (0, 0, 0), (0, 0, 0) ) for i < k2 do
      let j = k2-1 - i
      let u = us[i]
      let lhc3 = iterate lhc3 u vs[j]
      let lhc4 = iterate lhc4 u vs[j+1]
      in (lhc3, lhc4)
    let lhc4 = iterate lhc4 us[k2] vs[0]
    let (l2, lh2, hc2, cc2) = combine lhc3 lhc4

    in ((l1, lh1, l2, lh2), (hc1, cc1, hc2, cc2), (k1, k1+1, k2-1, k2))

  -- COPY TO SHARED MEMORY
  let cp2sh (i : i32) = #[unsafe]
    let str = i32.i64 m
    in ((us[i], us[str + i], us[2*str + i], us[3*str + i])
       ,(vs[i], vs[str + i], vs[2*str + i], vs[3*str + i]))

  -- 1. copy to shared memory coalesced
  let (uss, vss) = (0..<m) |> map i32.i64 |> map cp2sh |> unzip
  let (u1s, u2s, u3s, u4s) = unzip4 uss
  let (v1s, v2s, v3s, v4s) = unzip4 vss
  let ush = u1s ++ u2s ++ u3s ++ u4s |> opaque
  let vsh = v1s ++ v2s ++ v3s ++ v4s |> opaque

  -- 2. find the upper and lower four parts for each thread, and their indices
  let (llhs, hcccs, is) = map (CONV ush vsh) (0..<m) |> unzip3

  -- 3. construct the first of the additive arrays
  let (i1s, i2s, i3s, i4s) = unzip4 is
  let inds1 = i1s ++ i2s ++ i3s ++ i4s
  let (l1s, lh1s, l2s, lh2s) = unzip4 llhs
  let vals1 = l1s ++ lh1s ++ l2s ++ lh2s
  let arr1 = scatter (replicate (4*m) 0) inds1 vals1

  -- 4. construct the second of the additive arrays
  let (hc1s, cc1s, hc2s, cc2s) = unzip4 hcccs
  let vals2 = hc1s ++ cc1s ++ hc2s ++ cc2s
  let arr2 = scatter (replicate (4*m) 0) (map (+ 2) inds1) vals2

  -- 5. add the convolution parts
  in baddV2 arr1 arr2


--------------------------------------------------------------------------------
-- V3: Run multiple instances of V2 multiplications per block
--------------------------------------------------------------------------------

def convMulV3 [ipb][m] (us: [ipb*(4*m)]ui) (vs: [ipb*(4*m)]ui) : [ipb*(4*m)]ui=
  -- MULTIPLICATION BODY
  let CONV (us: []ui) (vs: []ui) (tid: i64)
      : ( (ui, ui, ui, ui), (ui, ui, ui, ui),
          (i64, i64, i64, i64), (i64, i64, i64, i64) ) = #[unsafe]
    let k1 = tid * 2
    let k1_start = (k1 / (4*m)) * (4*m)
    let (lhc1, lhc2) : ( (ui, ui, ui), (ui, ui, ui) ) =
      loop (lhc1,lhc2) = ( (0, 0, 0), (0, 0, 0) ) for i < (k1 + 1 - k1_start) do
      let j = k1 - i
      let u = us[i + k1_start]
      let lhc1 = iterate lhc1 u vs[j]
      let lhc2 = iterate lhc2 u vs[j+1]
      in (lhc1, lhc2)
    let lhc2 = iterate lhc2 us[k1+1] vs[k1_start]
    let (l1, lh1, hc1, cc1) = combine lhc1 lhc2

    let k2 = ipb*4*m - 1 - k1
    let k2_start = (k2 / (4*m)) * (4*m)
    let (lhc3, lhc4) : ( (ui, ui, ui), (ui, ui, ui) ) =
      loop (lhc3,lhc4) = ( (0, 0, 0), (0, 0, 0) ) for i < (k2 - k2_start) do
      let j = k2-1 - i
      let u = us[i + k2_start]
      let lhc3 = iterate lhc3 u vs[j]
      let lhc4 = iterate lhc4 u vs[j+1]
      in (lhc3, lhc4)
    let lhc4 = iterate lhc4 us[k2] vs[k2_start]
    let (l2, lh2, hc2, cc2) = combine lhc3 lhc4

    -- check if k1 or k2 is at the end of a segment (and if, set index to -1)
    let sege1 = (k1+2) % (4*m) != 0 |> i64.bool |> (\i -> i - 1)
    let sege2 = (k2+1) % (4*m) != 0 |> i64.bool |> (\i -> i - 1)
    let is2 = (sege1 | k1+2, sege1 | k1+3, sege2 | k2+1, sege2 | k2+2)

    in ((l1, lh1, l2, lh2), (hc1, cc1, hc2, cc2), (k1, k1+1, k2-1, k2), is2)

  -- COPY TO SHARED MEMORY
  let cp2sh (i : i32) = #[unsafe]
    let str = i32.i64 (ipb*m)
    in ((us[i], us[str + i], us[2*str + i], us[3*str + i])
       ,(vs[i], vs[str + i], vs[2*str + i], vs[3*str + i]))

  -- 1. copy to shared memory coalesced
  let (uss, vss) = (0..<ipb*m) |> map i32.i64 |> map cp2sh |> unzip
  let (u1s, u2s, u3s, u4s) = unzip4 uss
  let (v1s, v2s, v3s, v4s) = unzip4 vss
  let ush = u1s ++ u2s ++ u3s ++ u4s |> opaque
  let vsh = v1s ++ v2s ++ v3s ++ v4s |> opaque

  -- 2. find the upper and lower four parts for each thread, and their indices
  let (llhs, hcccs, is1, is2) = map (CONV ush vsh) (0..<ipb*m) |> unzip4

   -- 3. construct the first of the additive arrays
  let (i1s, i2s, i3s, i4s) = unzip4 is1
  let inds1 = i1s ++ i2s ++ i3s ++ i4s
  let (l1s, lh1s, l2s, lh2s) = unzip4 llhs
  let vals1 = l1s ++ lh1s ++ l2s ++ lh2s
  let arr1 = scatter (replicate (ipb*(4*m)) 0) inds1 vals1

  -- 4. construct the second of the additive arrays
  let (i5s, i6s, i7s, i8s) = unzip4 is2
  let inds2 = i5s ++ i6s ++ i7s ++ i8s
  let (hc1s, cc1s, hc2s, cc2s) = unzip4 hcccs
  let vals2 = hc1s ++ cc1s ++ hc2s ++ cc2s
  let arr2 = scatter (replicate (ipb*(4*m)) 0) inds2 vals2

  -- 5. add the convolution parts
  in baddV3 arr1 arr2


--------------------------------------------------------------------------------
-- V4: Run multiple instances of V1 multiplications per block
--------------------------------------------------------------------------------

def convMulV4 [ipb][m] (us: [ipb*(2*m)]ui) (vs: [ipb*(2*m)]ui) : [ipb*(2*m)]ui =
  -- MULTIPLICATION BODY
  let CONV (us: []ui) (vs: []ui) (tid: i64)
      : ( (ui, ui, ui), (ui, ui, ui) ) = #[unsafe]
    let k1 = tid
    let k1_start = (k1 / (2*m)) * (2*m)
    let lhc1 : (ui, ui, ui) = loop lhc = (0, 0, 0) for i < k1 + 1 - k1_start do
                              let j = k1 - i in iterate lhc us[i+k1_start] vs[j]
    let k2 = ipb*2*m-1 - k1
    let k2_start = (k2 / (2*m)) * (2*m)
    let lhc2 : (ui, ui, ui) = loop lhc = (0, 0, 0) for i < k2 + 1 - k2_start do
                              let j = k2 - i in iterate lhc us[i+k2_start] vs[j]
    in (lhc1, lhc2)

  -- COPY TO SHARED MEMORY
  let cp2sh (i : i32) = #[unsafe]
    let str = i32.i64 (ipb*m)
    in ( (us[i], us[str + i]), (vs[i], vs[str + i]) )

  -- 1. copy to shared memory coalesced
  let (uss, vss) = (0..<ipb*m) |> map i32.i64 |> map cp2sh |> unzip
  let (u1s, u2s) = unzip2 uss
  let (v1s, v2s) = unzip2 vss
  let ush = u1s ++ u2s |> opaque
  let vsh = v1s ++ v2s |> opaque

  -- 2. find two low, high, and carry parts for each thread
  let (lhcs1, lhcs2) = map (CONV ush vsh) (0..<ipb*m) |> unzip
  let (ls1, hs1, cs1) = unzip3 lhcs1
  let (ls2, hs2, cs2) = unzip3 <| reverse lhcs2
  let ls = ls1 ++ ls2 :> [ipb*(2*m)]ui
  let hs = hs1 ++ hs2 :> [ipb*(2*m)]ui
  let hs = map (\ i -> if i % (2*m) == 0 then 0 else hs[i-1]) (0..<ipb*(2*m))
  let cs = cs1 ++ cs2 :> [ipb*(2*m)]ui
  let cs = map (\ i -> if i % (2*m) <= 1 then 0 else cs[i-2]) (0..<ipb*(2*m))

  -- 3. add the low, high and carry parts
  in  baddV4 ls hs |> baddV4 cs


--------------------------------------------------------------------------------
-- Callers
--------------------------------------------------------------------------------

-- callers for one multiplication

entry oneMulV1 [n] (m: i64) (uss: [n][2*m]ui) (vss: [n][2*m]ui) : [n][2*m]ui =
  imap2Intra uss vss convMulV1

entry oneMulV2 [n] (m: i64) (uss: [n][4*m]ui) (vss: [n][4*m]ui) : [n][4*m]ui =
  imap2Intra uss vss convMulV2

entry oneMulV3 [n][ipb] (m: i64) (usss: [n][ipb][4*m]ui) (vsss: [n][ipb][4*m]ui) : [n][ipb][4*m]ui =
  let uss = map flatten usss :> [n][ipb*(4*m)]ui
  let vss = map flatten vsss :> [n][ipb*(4*m)]ui
  let wss = imap2Intra uss vss convMulV3
  in map unflatten wss

entry oneMulV4 [n][ipb] (m: i64) (usss: [n][ipb][2*m]ui) (vsss: [n][ipb][2*m]ui) : [n][ipb][2*m]ui =
  let uss = map flatten usss :> [n][ipb*(2*m)]ui
  let vss = map flatten vsss :> [n][ipb*(2*m)]ui
  let wss = imap2Intra uss vss convMulV4
  in map unflatten wss

-- callers for six multiplications, computing `(a * b)^6` for input `a` and `b`

entry sixMulV1 [n] (m: i64) (uss: [n][2*m]ui) (vss: [n][2*m]ui) : [n][2*m]ui =
  let wss1 = imap2Intra uss  vss  convMulV1
  let wss2 = imap2Intra wss1 wss1 convMulV1
  let wss3 = imap2Intra wss2 wss1 convMulV1
  let wss4 = imap2Intra wss3 wss1 convMulV1
  let wss5 = imap2Intra wss4 wss1 convMulV1
  in imap2Intra wss5 wss1 convMulV1

entry sixMulV2 [n] (m: i64) (uss: [n][4*m]ui) (vss: [n][4*m]ui) : [n][4*m]ui =
  let wss1 = imap2Intra uss  vss  convMulV2
  let wss2 = imap2Intra wss1 wss1 convMulV2
  let wss3 = imap2Intra wss2 wss1 convMulV2
  let wss4 = imap2Intra wss3 wss1 convMulV2
  let wss5 = imap2Intra wss4 wss1 convMulV2
  in imap2Intra wss5 wss1 convMulV2

entry sixMulV3 [n][ipb] (m: i64) (usss: [n][ipb][4*m]ui) (vsss: [n][ipb][4*m]ui) : [n][ipb][4*m]ui =
  let uss = map flatten usss :> [n][ipb*(4*m)]ui
  let vss = map flatten vsss :> [n][ipb*(4*m)]ui
  let wss1 = imap2Intra uss  vss  convMulV3
  let wss2 = imap2Intra wss1 wss1 convMulV3
  let wss3 = imap2Intra wss2 wss1 convMulV3
  let wss4 = imap2Intra wss3 wss1 convMulV3
  let wss5 = imap2Intra wss4 wss1 convMulV3
  let wss6 = imap2Intra wss5 wss1 convMulV3
  in map unflatten wss6

entry sixMulV4 [n][ipb] (m: i64) (usss: [n][ipb][2*m]ui) (vsss: [n][ipb][2*m]ui) : [n][ipb][2*m]ui =
  let uss = map flatten usss :> [n][ipb*(2*m)]ui
  let vss = map flatten vsss :> [n][ipb*(2*m)]ui
  let wss1 = imap2Intra uss  vss  convMulV4
  let wss2 = imap2Intra wss1 wss1 convMulV4
  let wss3 = imap2Intra wss2 wss1 convMulV4
  let wss4 = imap2Intra wss3 wss1 convMulV4
  let wss5 = imap2Intra wss4 wss1 convMulV4
  let wss6 = imap2Intra wss5 wss1 convMulV4
  in map unflatten wss6
