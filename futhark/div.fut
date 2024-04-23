import "helper"
import "add"
import "mul"

-------------------------------------------------------------
--- Big integer division (quotient) with base-type `u64`. ---
-------------------------------------------------------------

-- Index of most significant digit in big integer `u`.
def findk [m] (u: [m]u64) : i64 =
  map2 (\ x i -> if x != 0u64 then i else 0i64) u (iota m)
  |> reduce (\ acc i -> if i != 0i64 then i else acc) 0i64

-- Precision of big integer `u` (i.e. number of digits without zeroes in front).
def prec [m] (u: [m]u64) : i64 =
  findk u |> (+) 1

-- Shifts a big integer `u` by `n`, positive for right, negative for left.
def shift [m] (n: i64) (u: [m]u64) : [m]u64 =
  map (\ i -> let off = i - n
              in if off >= 0 && off < m then u[off] else 0u64) (iota m)

-- Constructs `(2^64)^n` as a `m` precision big integer.
def bpow (m: i64) (n: i64) : [m]u64 =
  singleton m 1u64 |> shift n

-- Addition of big integers `u` and `v`.
def add [m] (u: [m]u64) (v: [m]u64) : [m]u64 =
  badd64v2 u v

-- Difference between big integers `u` and `v` (false is `+` and true is `-`).
def sub [m] (u: [m]u64) (v: [m]u64) : ([m]u64, bool) =
  (u, true) -- TODO

-- Quotient of big integer `u` by one digit `d` (i.e. `u quo d`).
def quod [m] (u: [m]u64) (d: u64) : [m]u64 =
  u -- TODO this can be done using a scan (I think) - algorithms seems to exist

-- Multiplication of big integers `u` and `v`.
def mult [m] (u: [m]u64) (v: [m]u64) : [m]u64 =
  convMult64v2 u v

-- Multiplication of big integer `u` by one digit `d` (i.e. `u * d`).
def multd [m] (u: [m]u64) (d: u64) : [m]u64 =
  let (ls, hs) = unzip <| map (\ x -> (x * d, u64.mul_hi x d)) u
  let hs = map (\ i -> if i == 0 then 0 else hs[i-1]) (iota m)
  in add ls hs

-- Computes `(u * v) rem ((2^64)^e)` of big integers `u`, `v` and exponent `e`.
def multmod [m] (v: [m]u64) (w: [m]u64) (e: i64) : [m]u64 =
  let res = mult (take e v) (take e w)
  in map (\ i -> if i < e then res[i] else 0u64) (iota m)

-- Computes `(2^64)^h - (v * w)` (false is `+` and true is `-`)
def powdiff [m] (v: [m]u64) (w: [m]u64) (h: i64) (l: i64) : ([m]u64, bool) =
  let L = (prec v) + (prec w) - l + 1
  in if (ez v) || (ez w) then (bpow m h, false)
     else if L >= h then sub (bpow m h) (mult v w)
     else let P = multmod v w L
          in if ez P then (P, false)
             else if P[L-1] == 0 then (P, true)
             else sub (bpow m L) P

-- Takes an iterative refinement step (see paper for details)
def step [m] (v: [m]u64) (w: [m]u64) (h: i64) (n: i64) (l: i64) (g:i64): [m]u64=
  let (pd, sign) = powdiff v w (h-n) (l-g)
  let pdw_s = mult w pd |> shift (2*n - h)
  let w_s = shift n w
  in if sign then (sub w_s pdw_s |> fst) else add w_s pdw_s

-- Refinement strategy 1 (see paper for details)
def refine1 [m] (v: [m]u64) (w: [m]u64) (h: i64) (k: i64) (l: i64) : [m]u64 =
  let g = 1
  let h = h + g
  let (w, _) =
    loop (w, l) = (shift (h - k - l) w, l) while h - k > l do
    let w = step v w h 0 l 0
    let l = i64.min (2 * l - 1) (h - k)
    in (w, l)
  in shift (-g) w

-- Refinement strategy 2 (see paper for details)
def refine2 [m] (v: [m]u64) (w: [m]u64) (h: i64) (k: i64) (l: i64) : [m]u64 =
  let g = 2
  let (w, _) =
    loop (w, l) = (shift g w, l) while h - k > l do
    let n = i64.min (h - k + 1 - l) l
    let w = shift (-1) <| step v w (k + l + n + g) n l g
    let l = l + n - 1
    in (w, l)
  in shift (-g) w

-- Refinement strategy 3 (see paper for details)
def refine3 [m] (v: [m]u64) (w: [m]u64) (h: i64) (k: i64) (l: i64) : [m]u64 =
  let g = 2
  let (w, _) =
    loop (w, l) = (shift g w, l) while h - k > l do
    let n = i64.min (h - k + 1 - l) l
    let s = i64.max 0 (k - 2*l + 1 - g)
    let w = shift (-1) <| step (shift (-s) v) w (k + l + n - s + g) n l g
    let l = l + n - 1
    in (w, l)
  in shift (-g) w

-- Whole shifted inverse of big integer `v` by `h`.
def shinv [m] (v: [m]u64) (h: i64) : [m]u64 =
  let k = findk v
  -- handle the four special cases
  in if lt v (bpow m 1) then quod (bpow m h) v[0]
     else if lt (bpow m h) v then new m
     else if lt (bpow m h) (multd v 2) then singleton m 1
     else if eq v (bpow m h) then bpow m (h - k)
     -- form initial approximation
     else let l = 2 -- TODO this, or `i64.min k 2` ?
          let V = map (\ i -> v[k-l+i]) (iota (l + 1))
          let w = new m -- TODO compute w
           -- either return or refine initial approximation
           in if h - k <= l then shift (h - k - l) w
              else refine3 v w h k l

-- Divides big integer `u` by big integer `v`.
def div [m] (u: [m]u64) (v: [m]u64) : [m]u64 =
  -- TODO potential bug if `u = B^h` in the way `h` is computed
  let h = prec u
  -- pad big integers in advance for multiplications (TODO optimize)
  let u_p = map (\ i -> if i < m then u[i] else 0u64) (iota (m*2))
  let v_p = map (\ i -> if i < m then v[i] else 0u64) (iota (m*2))
  -- TODO handle delta
  in shinv v_p h |> mult u_p |> shift (-h) |> take m
