import "helper"

--------------------------------------------------
-- approach outlined by students of the DPP course
--------------------------------------------------
def add_op (ov1: bool, mx1: bool) (ov2: bool, mx2: bool) : (bool, bool) =
  ((ov1 && mx2) || ov2, mx1 && mx2)

def add32 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  let (res, cs) = imap2 as bs (\ a b -> let s = a + b
					in (s, (s < a, s == u32.highest))
			      ) |> unzip
  let (carries, _) = scan_exc add_op (false, true) cs |> unzip
  in imap2 res carries (\ x c -> x + u32.bool c)

-------------------------------------------------
-- optimized version 1 outlined in cambridge talk
-------------------------------------------------
-- bitwise; ov = `1`, mx = `2` and neutral element = 2
def carry_prop (c1: u32) (c2: u32) : u32 =
   (c1 & c2 & 2) | (((c1 & (c2 >> 1)) | c2) & 1)

def badd [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  let (res, cs) = imap2 as bs (\ a b -> let s = a + b
				        let b = u32.bool (s < a)
					let b = b | ((u32.bool (s == u32.highest)) << 1)
					in (s, b)
			       ) |> unzip
  let carries = scan_exc carry_prop 2 cs
  in imap2 res carries (\ r c -> r + (c & 1))

-------------------------------------------------
-- optimized version 2 outlined in cambridge talk
-------------------------------------------------
def badd0 [m] (ipn: i64) (n: i64) (as: [m]u32) (bs: [m]u32) : [m]u32 =
  let fs = map (\ i -> i % (n*4) == 0) (0..<m)
  let (res, cs) = imap2 as bs (\ a b -> let s = a + b
				        let b = u32.bool (s < a)
					let b = b | ((u32.bool (s == u32.highest)) << 1)
					in (s, b)
			       ) |> unzip
  let carries = seg_scan_exc carry_prop 2 fs cs
  in imap2 res carries (\ r c -> r + (c & 1))

def badd_opt [ipb][n] (as: [ipb*(4*n)]u32) (bs: [ipb*(4*n)]u32) : [ipb*(4*n)]u32 =
  let g = ipb * n
  let cp2sh (i: i32) =
    let g = i32.i64 g in
    ((as[i], as[g+i], as[2*g + i], as[3*g + i])
    ,(bs[i], bs[g+i], bs[2*g + i], bs[3*g + i]))
  let (ass, bss) = (0..<g) |> map i32.i64 |> map cp2sh |> unzip
  let (a1s, a2s, a3s, a4s) = unzip4 ass
  let (b1s, b2s, b3s, b4s) = unzip4 bss
  let ash = a1s ++ a2s ++ a3s ++ a4s
  let bsh = b1s ++ b2s ++ b3s ++ b4s
  in (badd0 ipb n ash bsh) :> [ipb*(4*n)]u32
