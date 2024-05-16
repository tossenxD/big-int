import "helper"

--------------------------------------------------------------------------------
--- Big Integer Addition
--------------------------------------------------------------------------------
--- The implementation builds upon ``Multiple-precision Integer Arithmetic''
--- by Amar Topalovic, Walter Restelli-Nielsen and Kristian Olesen, 2022,
--- published: https://futhark-lang.org/student-projects/dpp21-mpint.pdf, [1].
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Data Structures
--------------------------------------------------------------------------------

type ui = u64
type ct = u32

def HIGHEST  : ui            = u64.highest
def bits     : i64           = 64
def fromBool : (bool -> ui)  = u64.bool
def boolToCt : (bool -> ct)  = u32.bool
def fromCt   : (ct -> ui)    = u64.u32

-- type ui = u32
-- type ct = u32

-- def HIGHEST  : ui            = u32.highest
-- def bits     : i64           = 32
-- def fromBool : (bool -> ui)  = u32.bool
-- def boolToCt : (bool -> ct)  = u32.bool
-- def fromCt   : (ct -> ui)    = u32.u32


--------------------------------------------------------------------------------
-- Operators and helpers
--------------------------------------------------------------------------------

-- logical associative operator for addition

def addOp (ov1: bool, mx1: bool) (ov2: bool, mx2: bool) : (bool, bool) =
  ((ov1 && mx2) || ov2, mx1 && mx2)

def addOpE : (bool, bool) = (false, true)

-- bitwise and segmented versions where ov = `1`, mx = `2`, and flag = `3`

def carryProp (c1: ct) (c2: ct) : ct =
  (c1 & c2 & 2) | (((c1 & (c2 >> 1)) | c2) & 1)

def carryPropSeg (c1: ct) (c2: ct) : ct =
  if (c2 & 4) == 4 then c2 else carryProp c1 c2 | (c1 & 4)

def carryPropE : ct = 2

-- augmented carry for two elements

def carryAug (r : ui) (a : ui) : ct =
  (boolToCt (r < a)) | ((boolToCt (r == HIGHEST)) << 1)

-- exclusive scan

def scanExc [n] 't (g: (t -> t -> t)) (ne: t)  (cs: [n]t) : [n]t =
  let cs = scan g ne cs
  in map (\ i -> if i == 0 then ne else cs[i-1]) (0..<n)


--------------------------------------------------------------------------------
-- V0: Approach outlined by [1]
--------------------------------------------------------------------------------

def add [m] (us: [m]ui) (vs: [m]ui) : [m]ui =
  -- 1. compute sums and carries
  let (ws, cs) = map2 (\ u v -> let w = u + v
                                in (w, (w < u, w == HIGHEST))
                      ) us vs |> unzip

  -- 2. propagate carries
  let (pcs, _) = scanExc addOp addOpE cs |> unzip

  -- 3. add carries to sums
  in map2  (\ w c -> w + fromBool c) ws pcs


--------------------------------------------------------------------------------
-- V1: Using bitwise instead of logical operators
--------------------------------------------------------------------------------

def baddV1 [m] (us: [m]ui) (vs: [m]ui) : [m]ui =
  -- 1. compute sums and carries
  let (ws, cs) = map2 (\ u v -> let w = u+v in (w, carryAug w u)) us vs |> unzip

  -- 2. propagate carries
  let pcs = scanExc carryProp carryPropE cs

  -- 3. add carries to sums
  in map2 (\ w c -> w + fromCt (c & 1)) ws pcs


--------------------------------------------------------------------------------
-- V2: Effeciently sequentialize the parallelism in excess (fixed factor of 4)
--------------------------------------------------------------------------------

def baddV2 [m] (us: [4*m]ui) (vs: [4*m]ui) : [4*m]ui =
  -- ADDITION BODY
  let baddV2Run (us: []ui) (vs: []ui) : []ui = #[unsafe]
    -- 1. compute sums, carries, and register-level prefix sum
    let (ws, cs, accs) = unzip3 <| imap (0..<m)
      (\ i -> let (u1, u2, u3, u4) = (us[i*4], us[i*4+1], us[i*4+2], us[i*4+3])
              let (v1, v2, v3, v4) = (vs[i*4], vs[i*4+1], vs[i*4+2], vs[i*4+3])
              let (w1, w2, w3, w4) = (u1 + v1, u2 + v2, u3 + v3, u4 + v4)
              let (c1, c2, c3, c4) = (carryAug w1 u1, carryAug w2 u2,
                                      carryAug w3 u3, carryAug w4 u4)
              let acc = carryProp c1 <| carryProp c2 <| carryProp c3 c4
              in ((w1, w2, w3, w4), (c1, c2, c3, c4), acc))

    -- 2. propagate carries
    let pcs = scanExc carryProp carryPropE accs

    -- 3. distribute carries over register-level prefix sum, and add them to sum
    in flatten <| imap3 ws cs pcs
      (\ (w1, w2, w3, w4) (c1, c2, c3, _) acc1 ->
         let acc2 = carryProp acc1 c1
         let acc3 = carryProp acc2 c2
         let acc4 = carryProp acc3 c3
         in [w1 + fromCt (acc1 & 1), w2 + fromCt (acc2 & 1),
             w3 + fromCt (acc3 & 1), w4 + fromCt (acc4 & 1)])

  -- COPY TO SHARED MEMORY
  let cp2sh (i : i32) = #[unsafe]
    let str = i32.i64 m
    in ((us[i], us[str + i], us[2*str + i], us[3*str + i])
       ,(vs[i], vs[str + i], vs[2*str + i], vs[3*str + i]))

  -- 1. copy to shared memory coalesced
  let (uss, vss) = (0..<m) |> map i32.i64 |> map cp2sh |> unzip
  let (u1s, u2s, u3s, u4s) = unzip4 uss
  let (v1s, v2s, v3s, v4s) = unzip4 vss
  let ush = u1s ++ u2s ++ u3s ++ u4s
  let vsh = v1s ++ v2s ++ v3s ++ v4s

  -- 2. run the addition body
  in baddV2Run ush vsh :> [4*m]ui


--------------------------------------------------------------------------------
-- V3: Run multiple instances of addition per block
--------------------------------------------------------------------------------

def baddV3 [ipb][m] (us: [ipb*(4*m)]ui) (vs: [ipb*(4*m)]ui) : [ipb*(4*m)]ui =
  -- ADDITION BODY
  let baddV3Run (us: []ui) (vs: []ui) : []ui = #[unsafe]
    -- 1. compute sums, carries, flags, and register-level prefix sum
    let (ws, cs, accs) = unzip3 <| imap (0..<ipb*m)
      (\ i -> let (u1, u2, u3, u4) = (us[i*4], us[i*4+1], us[i*4+2], us[i*4+3])
              let (v1, v2, v3, v4) = (vs[i*4], vs[i*4+1], vs[i*4+2], vs[i*4+3])
              let (w1, w2, w3, w4) = (u1 + v1, u2 + v2, u3 + v3, u4 + v4)
              let (c1, c2, c3, c4) = (carryAug w1 u1, carryAug w2 u2,
                                      carryAug w3 u3, carryAug w4 u4)
              let c1 = (boolToCt (i % m == 0)) << 2 | c1
              let acc = carryProp c1 <| carryProp c2 <| carryProp c3 c4
              in ((w1, w2, w3, w4), (c1, c2, c3, c4), acc))

    -- 2. propagate carries
    let pcs = scanExc carryPropSeg carryPropE accs

    -- 3. distribute carries over register-level prefix sum, and add them to sum
    in flatten <| imap3 ws cs pcs
      (\ (w1, w2, w3, w4) (c1, c2, c3, _) acc1 ->
         let acc2 = carryProp acc1 c1
         let acc3 = carryProp acc2 c2
         let acc4 = carryProp acc3 c3
         in [w1 + fromCt (acc1 & 1), w2 + fromCt (acc2 & 1),
             w3 + fromCt (acc3 & 1), w4 + fromCt (acc4 & 1)])

  -- COPY TO SHARED MEMORY
  let cp2sh (i : i32) = #[unsafe]
    let str = i32.i64 (ipb*m)
    in ((us[i], us[str + i], us[2*str + i], us[3*str + i])
       ,(vs[i], vs[str + i], vs[2*str + i], vs[3*str + i]))

  -- 1. copy to shared memory coalesced
  let (uss, vss) = (0..<ipb*m) |> map i32.i64 |> map cp2sh |> unzip
  let (u1s, u2s, u3s, u4s) = unzip4 uss
  let (v1s, v2s, v3s, v4s) = unzip4 vss
  let ush = u1s ++ u2s ++ u3s ++ u4s
  let vsh = v1s ++ v2s ++ v3s ++ v4s

  -- 2. run the addition body
  in baddV3Run ush vsh :> [ipb*(4*m)]ui


--------------------------------------------------------------------------------
-- Callers
--------------------------------------------------------------------------------

-- callers for one addition

entry oneAddV0 [n][m] (uss: [n][m]ui) (vss: [n][m]ui) : [n][m]ui =
  imap2Intra uss vss add

entry oneAddV1 [n][m] (uss: [n][m]ui) (vss: [n][m]ui) : [n][m]ui =
  imap2Intra uss vss baddV1

entry oneAddV2 [n] (m: i64) (uss: [n][4*m]ui) (vss: [n][4*m]ui) : [n][4*m]ui =
  imap2Intra uss vss baddV2

entry oneAddV3 [n][ipb] (m: i64) (usss: [n][ipb][4*m]ui) (vsss: [n][ipb][4*m]ui) : [n][ipb][4*m]ui =
  let uss = map flatten usss :> [n][ipb*(4*m)]ui
  let vss = map flatten vsss :> [n][ipb*(4*m)]ui
  let wss = imap2Intra uss vss baddV3
  in map unflatten wss

-- callers for ten additions, computing `9*uss + vss` for input `uss` and `vss`

entry tenAddV0 [n][m] (uss: [n][m]ui) (vss: [n][m]ui) : [n][m]ui =
  let wss = imap2Intra uss vss add
  let wss = imap2Intra uss wss add
  let wss = imap2Intra uss wss add
  let wss = imap2Intra uss wss add
  let wss = imap2Intra uss wss add
  let wss = imap2Intra uss wss add
  let wss = imap2Intra uss wss add
  let wss = imap2Intra uss wss add
  let wss = imap2Intra uss wss add
  in imap2Intra uss wss add

entry tenAddV1 [n][m] (uss: [n][m]ui) (vss: [n][m]ui) : [n][m]ui =
  let wss = imap2Intra uss vss baddV1
  let wss = imap2Intra uss wss baddV1
  let wss = imap2Intra uss wss baddV1
  let wss = imap2Intra uss wss baddV1
  let wss = imap2Intra uss wss baddV1
  let wss = imap2Intra uss wss baddV1
  let wss = imap2Intra uss wss baddV1
  let wss = imap2Intra uss wss baddV1
  let wss = imap2Intra uss wss baddV1
  in imap2Intra uss wss baddV1

entry tenAddV2 [n] (m: i64) (uss: [n][4*m]ui) (vss: [n][4*m]ui) : [n][4*m]ui =
  let wss = imap2Intra uss vss baddV2
  let wss = imap2Intra uss wss baddV2
  let wss = imap2Intra uss wss baddV2
  let wss = imap2Intra uss wss baddV2
  let wss = imap2Intra uss wss baddV2
  let wss = imap2Intra uss wss baddV2
  let wss = imap2Intra uss wss baddV2
  let wss = imap2Intra uss wss baddV2
  let wss = imap2Intra uss wss baddV2
  in imap2Intra uss wss baddV2

entry tenAddV3 [n][ipb] (m: i64) (usss: [n][ipb][4*m]ui) (vsss: [n][ipb][4*m]ui) : [n][ipb][4*m]ui =
  let uss = map flatten usss :> [n][ipb*(4*m)]ui
  let vss = map flatten vsss :> [n][ipb*(4*m)]ui
  let wss = imap2Intra uss vss baddV3
  let wss = imap2Intra uss wss baddV3
  let wss = imap2Intra uss wss baddV3
  let wss = imap2Intra uss wss baddV3
  let wss = imap2Intra uss wss baddV3
  let wss = imap2Intra uss wss baddV3
  let wss = imap2Intra uss wss baddV3
  let wss = imap2Intra uss wss baddV3
  let wss = imap2Intra uss wss baddV3
  let wss = imap2Intra uss wss baddV3
  in map unflatten wss
