import "helper"

--------------------------------------------------------------------------------
-- operators / helper functions
--------------------------------------------------------------------------------

-- logical associative operator for addition
def add_op (ov1: bool, mx1: bool) (ov2: bool, mx2: bool) : (bool, bool) =
  ((ov1 && mx2) || ov2, mx1 && mx2)

-- bitwise version; ov = `1`, mx = `2` and neutral element = 2
def carry_prop32 (c1: u32) (c2: u32) : u32 =
  (c1 & c2 & 2) | (((c1 & (c2 >> 1)) | c2) & 1)

def carry_prop64 (c1: u64) (c2: u64) : u64 =
  (c1 & c2 & 2) | (((c1 & (c2 >> 1)) | c2) & 1)

-- computes the un-propagated bitwise carry for a unit
def initc32 (r : u32) (a : u32) : u32 =
  (u32.bool (r < a)) | ((u32.bool (r == u32.highest)) << 1)

def initc64 (r : u64) (a : u64) : u64 =
  (u64.bool (r < a)) | ((u64.bool (r == u64.highest)) << 1)



--------------------------------------------------------------------------------
-- approach outlined by students as part of the DPP course
--------------------------------------------------------------------------------

entry add32 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  -- 1. compute result and carry
  let (res, cs) = map2 (\ a b -> let r = a + b
                                 in (r, (r < a, r == u32.highest))
                       ) as bs |> unzip

  -- 2. propagate carries
  let (carries, _) = scan_exc add_op (false, true) cs |> unzip

  -- add carries to result
  in imap2 res carries (\ r c -> r + u32.bool c)

entry add64 [n] (as: [n]u64) (bs: [n]u64) : [n]u64 =
  -- 1. compute result and carry
  let (res, cs) = map2 (\ a b -> let r = a + b
                                 in (r, (r < a, r == u64.highest))
                       ) as bs |> unzip

  -- 2. propagate carries
  let (carries, _) = scan_exc add_op (false, true) cs |> unzip

  -- add carries to result
  in imap2 res carries (\ r c -> r + u64.bool c)



--------------------------------------------------------------------------------
-- optimized approach using bitwise instead of logical operators
--------------------------------------------------------------------------------

entry badd32v1 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  -- 1. compute result and carry
  let (res, cs) = map2 (\ a b -> let r = a + b in (r,initc32 r a)) as bs |>unzip

  -- 2. propagate carries
  let carries = scan_exc carry_prop32 2 cs

  -- 3. add carries to result
  in imap2 res carries (\ r c -> r + (c & 1))

entry badd64v1 [n] (as: [n]u64) (bs: [n]u64) : [n]u64 =
  -- 1. compute result and carry
  let (res, cs) = map2 (\ a b -> let r = a + b in (r,initc64 r a)) as bs |>unzip

  -- 2. propagate carries
  let carries = scan_exc carry_prop64 2 cs

  -- 3. add carries to result
  in imap2 res carries (\ r c -> r + (c & 1))



--------------------------------------------------------------------------------
-- optimized with bitwise operator and effecient sequentialization
--------------------------------------------------------------------------------

entry badd32v2 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  -- ADDITION BODY
  let badd32v2Run0 (n: i64) (as: []u32) (bs: []u32) : [n*4]u32 =
    -- 1. compute result, carry and register scan
    let (rs, cs, accs) = unzip3 <| imap (0..<n)
      (\ i -> let (a1, a2, a3, a4) = (as[i*4], as[i*4+1], as[i*4+2], as[i*4+3])
              let (b1, b2, b3, b4) = (bs[i*4], bs[i*4+1], bs[i*4+2], bs[i*4+3])
              let (r1, r2, r3, r4) = (a1 + b1, a2 + b2,   a3 + b3,   a4 + b4)
              let (c1, c2, c3, c4) =
                (initc32 r1 a1, initc32 r2 a2, initc32 r3 a3, initc32 r4 a4)
              let acc = carry_prop32 c1 <| carry_prop32 c2<| carry_prop32 c3 c4
              in ((r1, r2, r3, r4), (c1, c2, c3, c4), acc))
    -- 2. propagate carries
    let carries = scan_exc carry_prop32 2 accs
    -- 3. add carries to results
    in flatten <| imap3 rs cs carries
      (\ (r1, r2, r3, r4) (c1, c2, c3, _) acc1 ->
         let acc2 = carry_prop32 acc1 c1
         let acc3 = carry_prop32 acc2 c2
         let acc4 = carry_prop32 acc3 c3
         in [r1 + (acc1 & 1), r2 + (acc2 & 1), r3 + (acc3 & 1), r4 + (acc4 & 1)])

  -- COPY TO SHARED MEMORY
  let badd32v2Run [n] (as: [4*n]u32) (bs: [4*n]u32) : [4*n]u32 =
    let cp2sh (i : i32) =
      let n = i32.i64 n in
      ((as[i], as[n+i], as[2*n + i], as[3*n + i])
      ,(bs[i], bs[n+i], bs[2*n + i], bs[3*n + i]))
    -- 1. copy to shared/register memory coalesced
    let (ass, bss) = (0..<n) |> map i32.i64 |> map cp2sh |> unzip
    let (a1s, a2s, a3s, a4s) = unzip4 ass
    let (b1s, b2s, b3s, b4s) = unzip4 bss
    let ash = a1s ++ a2s ++ a3s ++ a4s
    let bsh = b1s ++ b2s ++ b3s ++ b4s
    -- 2. run above function
    in (badd32v2Run0 n ash bsh) :> [4*n]u32

  -- ADD PADDING AND CALL THE ABOVE FUNCTIONS
  let p = 4 - (n % 4)
  let pz = map (\_ -> 0) (0..<p)
  let as = as ++ pz :> [4*((n + p) / 4)]u32
  let bs = bs ++ pz :> [4*((n + p) / 4)]u32
  in take n <| badd32v2Run as bs

entry badd64v2 [n] (as: [n]u64) (bs: [n]u64) : [n]u64 =
  -- ADDITION BODY
  let badd64v2Run0 (n: i64) (as: []u64) (bs: []u64) : [n*4]u64 =
    -- 1. compute result, carry and register scan
    let (rs, cs, accs) = unzip3 <| imap (0..<n)
      (\ i -> let (a1, a2, a3, a4) = (as[i*4], as[i*4+1], as[i*4+2], as[i*4+3])
              let (b1, b2, b3, b4) = (bs[i*4], bs[i*4+1], bs[i*4+2], bs[i*4+3])
              let (r1, r2, r3, r4) = (a1 + b1, a2 + b2,   a3 + b3,   a4 + b4)
              let (c1, c2, c3, c4) =
                (initc64 r1 a1, initc64 r2 a2, initc64 r3 a3, initc64 r4 a4)
              let acc = carry_prop64 c1 <| carry_prop64 c2<| carry_prop64 c3 c4
              in ((r1, r2, r3, r4), (c1, c2, c3, c4), acc))
    -- 2. propagate carries
    let carries = scan_exc carry_prop64 2 accs
    -- 3. add carries to results
    in flatten <| imap3 rs cs carries
      (\ (r1, r2, r3, r4) (c1, c2, c3, _) acc1 ->
         let acc2 = carry_prop64 acc1 c1
         let acc3 = carry_prop64 acc2 c2
         let acc4 = carry_prop64 acc3 c3
         in [r1 + (acc1 & 1), r2 + (acc2 & 1), r3 + (acc3 & 1), r4 + (acc4 & 1)])

  -- COPY TO SHARED MEMORY
  let badd64v2Run [n] (as: [4*n]u64) (bs: [4*n]u64) : [4*n]u64 =
    let cp2sh (i : i32) =
      let n = i32.i64 n in
      ((as[i], as[n+i], as[2*n + i], as[3*n + i])
      ,(bs[i], bs[n+i], bs[2*n + i], bs[3*n + i]))
    -- 1. copy to shared/register memory coalesced
    let (ass, bss) = (0..<n) |> map i32.i64 |> map cp2sh |> unzip
    let (a1s, a2s, a3s, a4s) = unzip4 ass
    let (b1s, b2s, b3s, b4s) = unzip4 bss
    let ash = a1s ++ a2s ++ a3s ++ a4s
    let bsh = b1s ++ b2s ++ b3s ++ b4s
    -- 2. run above function
    in (badd64v2Run0 n ash bsh) :> [4*n]u64

  -- ADD PADDING AND CALL THE ABOVE FUNCTIONS
  let p = 4 - (n % 4)
  let pz = map (\_ -> 0) (0..<p)
  let as = as ++ pz :> [4*((n + p) / 4)]u64
  let bs = bs ++ pz :> [4*((n + p) / 4)]u64
  in take n <| badd64v2Run as bs



--------------------------------------------------------------------------------
-- optimized bitwise, sequentialization and multiple instances per block
--------------------------------------------------------------------------------

entry badd32v3 [m] (ipb: i64) (as: [m]u32) (bs: [m]u32) : [m]u32 =
  -- ADDITION BODY
  let badd32v3Run0 (n: i64) (ipb: i64) (as: []u32) (bs: []u32) : [ipb*n*4]u32 =
    -- 1. compute result, carry and register scan
    let (rs, cs, accs, flgs) = unzip4 <| imap (0..<ipb*n)
      (\ i -> let (a1, a2, a3, a4) = (as[i*4], as[i*4+1], as[i*4+2], as[i*4+3])
              let (b1, b2, b3, b4) = (bs[i*4], bs[i*4+1], bs[i*4+2], bs[i*4+3])
              let (r1, r2, r3, r4) = (a1 + b1, a2 + b2,   a3 + b3,   a4 + b4)
              let (c1, c2, c3, c4) =
                (initc32 r1 a1, initc32 r2 a2, initc32 r3 a3, initc32 r4 a4)
              let acc = carry_prop32 c1 <| carry_prop32 c2<| carry_prop32 c3 c4
              let flg = i % n == 0
              in ((r1, r2, r3, r4), (c1, c2, c3, c4), acc, flg))
    -- 2. propagate carries
    let carries = seg_scan_exc carry_prop32 2 flgs accs
    -- 3. add carries to results
    in flatten <| imap3 rs cs carries
      (\ (r1, r2, r3, r4) (c1, c2, c3, _) acc1 ->
         let acc2 = carry_prop32 acc1 c1
         let acc3 = carry_prop32 acc2 c2
         let acc4 = carry_prop32 acc3 c3
         in [r1 + (acc1 & 1), r2 + (acc2 & 1), r3 + (acc3 & 1), r4 + (acc4 & 1)])

  -- COPY TO SHARED MEMORY
  let badd32v3Run [ipb][n] (as: [ipb*(4*n)]u32) (bs: [ipb*(4*n)]u32) : [ipb*(4*n)]u32 =
    let cp2sh (i : i32) =
      let g = i32.i64 (n*ipb) in
      ((as[i], as[g+i], as[2*g + i], as[3*g + i])
      ,(bs[i], bs[g+i], bs[2*g + i], bs[3*g + i]))
    -- 1. copy to shared/register memory coalesced
    let (ass, bss) = (0..<n*ipb) |> map i32.i64 |> map cp2sh |> unzip
    let (a1s, a2s, a3s, a4s) = unzip4 ass
    let (b1s, b2s, b3s, b4s) = unzip4 bss
    let ash = a1s ++ a2s ++ a3s ++ a4s
    let bsh = b1s ++ b2s ++ b3s ++ b4s
    -- 2. run above function
    in (badd32v3Run0 n ipb ash bsh) :> [ipb*(4*n)]u32

  -- ADD PADDING AND CALL THE ABOVE FUNCTIONS
  let p  = 4 - (m % 4)
  let pz = map (\_ -> 0) (0..<p)
  let mp = m + p
  --let ipb= (128 + (mp/4) - 1) / (mp/4) -- ceil(128/(mp/4))
  let n  = mp / ipb
  let as = as ++ pz :> [ipb * (4*(n / 4))]u32
  let bs = bs ++ pz :> [ipb * (4*(n / 4))]u32
  in take m <| badd32v3Run as bs

entry badd64v3 [m] (ipb: i64) (as: [m]u64) (bs: [m]u64) : [m]u64 =
  -- ADDITION BODY
  let badd64v3Run0 (n: i64) (ipb: i64) (as: []u64) (bs: []u64) : [ipb*n*4]u64 =
    -- 1. compute result, carry and register scan
    let (rs, cs, accs, flgs) = unzip4 <| imap (0..<ipb*n)
      (\ i -> let (a1, a2, a3, a4) = (as[i*4], as[i*4+1], as[i*4+2], as[i*4+3])
              let (b1, b2, b3, b4) = (bs[i*4], bs[i*4+1], bs[i*4+2], bs[i*4+3])
              let (r1, r2, r3, r4) = (a1 + b1, a2 + b2,   a3 + b3,   a4 + b4)
              let (c1, c2, c3, c4) =
                (initc64 r1 a1, initc64 r2 a2, initc64 r3 a3, initc64 r4 a4)
              let acc = carry_prop64 c1 <| carry_prop64 c2<| carry_prop64 c3 c4
              let flg = i % n == 0
              in ((r1, r2, r3, r4), (c1, c2, c3, c4), acc, flg))
    -- 2. propagate carries
    let carries = seg_scan_exc carry_prop64 2 flgs accs
    -- 3. add carries to results
    in flatten <| imap3 rs cs carries
      (\ (r1, r2, r3, r4) (c1, c2, c3, _) acc1 ->
         let acc2 = carry_prop64 acc1 c1
         let acc3 = carry_prop64 acc2 c2
         let acc4 = carry_prop64 acc3 c3
         in [r1 + (acc1 & 1), r2 + (acc2 & 1), r3 + (acc3 & 1), r4 + (acc4 & 1)])

  -- COPY TO SHARED MEMORY
  let badd64v3Run [ipb][n] (as: [ipb*(4*n)]u64) (bs: [ipb*(4*n)]u64) : [ipb*(4*n)]u64 =
    let cp2sh (i : i32) =
      let g = i32.i64 (n*ipb) in
      ((as[i], as[g+i], as[2*g + i], as[3*g + i])
      ,(bs[i], bs[g+i], bs[2*g + i], bs[3*g + i]))
    -- 1. copy to shared/register memory coalesced
    let (ass, bss) = (0..<n*ipb) |> map i32.i64 |> map cp2sh |> unzip
    let (a1s, a2s, a3s, a4s) = unzip4 ass
    let (b1s, b2s, b3s, b4s) = unzip4 bss
    let ash = a1s ++ a2s ++ a3s ++ a4s
    let bsh = b1s ++ b2s ++ b3s ++ b4s
    -- 2. run above function
    in (badd64v3Run0 n ipb ash bsh) :> [ipb*(4*n)]u64

  -- ADD PADDING AND CALL THE ABOVE FUNCTIONS
  let p  = 4 - (m % 4)
  let pz = map (\_ -> 0) (0..<p)
  let mp = m + p
  --let ipb= (128 + (mp/4) - 1) / (mp/4) -- ceil(128/(mp/4))
  let n  = mp / ipb
  let as = as ++ pz :> [ipb * (4*(n / 4))]u64
  let bs = bs ++ pz :> [ipb * (4*(n / 4))]u64
  in take m <| badd64v3Run as bs

--
--- wrapper functions with error handling (main v3 functions are tricky to call)
--

entry badd32v3Wrapper [n][m] (as: [n][m]u32) (bs: [n][m]u32) : [n][m]u32 =
  let ipb = if m > 4 then (128 + (m/4) - 1) / (m/4) else 1 -- ceil(128/(m/4))
  let ipb = if ipb > n || (n % ipb) > 0 then 1 else ipb
  let as  = flatten as |> unflatten :> [n/ipb][ipb*m]u32
  let bs  = flatten bs |> unflatten :> [n/ipb][ipb*m]u32
  in map2 (badd32v3 ipb) as bs |> flatten |> unflatten :> [n][m]u32

entry badd64v3Wrapper [n][m] (as: [n][m]u64) (bs: [n][m]u64) : [n][m]u64 =
  let ipb = if m > 4 then (128 + (m/4) - 1) / (m/4) else 1 -- ceil(128/(m/4))
  let ipb = if ipb > n || (n % ipb) > 0 then 1 else ipb
  let as  = flatten as |> unflatten :> [n/ipb][ipb*m]u64
  let bs  = flatten bs |> unflatten :> [n/ipb][ipb*m]u64
  in map2 (badd64v3 ipb) as bs |> flatten |> unflatten :> [n][m]u64
