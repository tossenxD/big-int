import "helper"

------------------------------------------------------
-- operators / helper functions
------------------------------------------------------

-- logical associative operator for addition
def add_op (ov1: bool, mx1: bool) (ov2: bool, mx2: bool) : (bool, bool) =
  ((ov1 && mx2) || ov2, mx1 && mx2)

-- bitwise version; ov = `1`, mx = `2` and neutral element = 2
def carry_prop (c1: u32) (c2: u32) : u32 =
  (c1 & c2 & 2) | (((c1 & (c2 >> 1)) | c2) & 1)

-- computes the bitwise carry for a unit
def initc (r : u32) (a : u32) : u32 =
  (u32.bool (r < a)) | ((u32.bool (r == u32.highest)) << 1)



------------------------------------------------------
-- version 0; straightforward approach
------------------------------------------------------

def add32 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  -- 1. compute result and carry
  let (res, cs) = map2 (\ a b -> let r = a + b
                                 in (r, (r < a, r == u32.highest))
                       ) as bs |> unzip

  -- 2. propagate carries
  let (carries, _) = scan_exc add_op (false, true) cs |> unzip

  -- add carries to result
  in imap2 res carries (\ r c -> r + u32.bool c)

  
  
------------------------------------------------------
-- version 1; bitwise instead of logical operators
------------------------------------------------------

def badd1 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  -- 1. compute result and carry
  let (res, cs) = map2 (\ a b -> let r = a + b
                                 in (r, initc r a)
                       ) as bs |> unzip

  -- 2. propagate carries
  let carries = scan_exc carry_prop 2 cs
  
  -- 3. add carries to result
  in imap2 res carries (\ r c -> r + (c & 1))


  
------------------------------------------------------
-- version 2; sequentialized with 4 units per thread
------------------------------------------------------
  
def badd2Run [n] (as: [4*n]u32) (bs: [4*n]u32) : [4*n]u32 =
  -- 1. compute result, carry and register scan
  let initc (r : u32) (a : u32) : u32 =
    (u32.bool (r < a)) | ((u32.bool (r == u32.highest)) << 1)
  let (rs, cs, accs) =
    map (\ i -> let ass = (as[i*4], as[i*4+1], as[i*4+2], as[i*4+3])
                let bss = (bs[i*4], bs[i*4+1], bs[i*4+2], bs[i*4+3])
                let rss = (ass.0+bss.0, ass.1+bss.1, ass.2+bss.2, ass.3+bss.3)
                let css = (initc rss.0 ass.0, initc rss.1 ass.1,
                           initc rss.2 ass.2, initc rss.3 ass.3)
                let acc = carry_prop css.0 <| carry_prop css.1
                          <| carry_prop css.2 css.3
                in (rss, bss, acc)
        ) (iota n) |> unzip3

  -- 2. propagate carries
  let carries = scan_exc carry_prop 2 accs

  -- 3. add carries to results
  in map3 (\ rss css acc0 ->
             let acc1 = carry_prop acc0 css.0
             let acc2 = carry_prop acc1 css.1
             let acc3 = carry_prop acc2 css.2
             in [rss.0 + (acc0 & 1), rss.1 + (acc1 & 1),
                 rss.2 + (acc2 & 1), rss.3 + (acc3 & 1)]
          )  rs cs carries |> flatten :> [4*n]u32

def badd2 [n] (as: [4*n]u32) (bs: [4*n]u32) : [4*n]u32 =
  let cp2sh (i : i32) =
    let n = i32.i64 n in
    ((as[i], as[n+i], as[2*n + i], as[3*n + i])
    ,(bs[i], bs[n+i], bs[2*n + i], bs[3*n + i]))
  let (ass, bss) = (0..<n) |> map i32.i64 |> map cp2sh |> unzip
  let (a1s, a2s, a3s, a4s) = unzip4 ass
  let (b1s, b2s, b3s, b4s) = unzip4 bss
  let ash = a1s ++ a2s ++ a3s ++ a4s :> [4*n]u32
  let bsh = b1s ++ b2s ++ b3s ++ b4s :> [4*n]u32
  in (badd2Run ash bsh) :> [4*n]u32
  

  
------------------------------------------------------
-- version 3; blocks can handle multiple instances
------------------------------------------------------
