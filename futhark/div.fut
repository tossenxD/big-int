import "helper"
import "add"
import "sub"
import "mul"

------------------------------------------------------------
--- Big integer division (quotient) with base-type `u64` ---
------------------------------------------------------------
-- The implementation is based on the paper "Efficient   ---
-- Generic Quotients Using Exact Arithmetic" by Stephen  ---
-- M. Watt: https://arxiv.org/pdf/2304.01753.pdf         ---
------------------------------------------------------------

-- Shifts a big integer `u` by `n`, positive for right, negative for left.
def shift [m] (n: i64) (u: [m]u64) : [m]u64 =
  map (\ i -> let off = i - n
              in if off >= 0 && off < m then u[off] else 0u64) (iota m)

-- Constructs `(2^64)^n` as a `m` precision big integer.
def bpow (m: i64) (n: i64) : [m]u64 =
  singleton64 m 1u64 |> shift n

-- Checks whether `u < B^i`, where `B = 2^64`.
def ltBpow [m] (u: [m]u64) (i: i64) : bool =
  map2 (\ x j -> x == 0 || j < i) u (iota m) |> reduce (&&) true

-- Checks whether `u > B^i`, where `B = 2^64`.
def gtBpow [m] (u: [m]u64) (i: i64) : bool =
  map2 (\ x j -> (x > 1 && j == i) || (x > 0 && j > i)) u (iota m)
  |> reduce (||) false

-- Checks whether `u == B^i`, where `B = 2^64`.
def eqBpow [m] (u: [m]u64) (i: i64) : bool =
  map2 (\ x j -> (x == 0 || j != i) || (x == 1 || j == i)) u (iota m)
  |> reduce (&&) true |> (&&) (m > i)

-- Index of most significant nonzero digit in big integer `u`.
def findk [m] (u: [m]u64) : i64 =
  map2 (\ x i -> if x != 0u64 then i else 0i64) u (iota m)
  |> reduce (\ acc i -> if i != 0i64 then i else acc) 0i64

-- Index `i` such that `u <= B^i`where `B = 2^64`.
def findh [m] (u: [m]u64) : i64 =
  let i = findk u -- TODO since i+1 always holds, the else branch is always safe
  in if eqBpow u i then i else i + 1

-- Precision of big integer `u` (i.e. number of digits minus leading zeroes).
def prec [m] (u: [m]u64) : i64 =
  findk u |> (+) 1

-- Addition of big integers `u` and `v`.
def add [m] (u: [m]u64) (v: [m]u64) : [m]u64 =
  badd64v2 u v

-- Difference between big integers `u` and `v` (false is `+` and true is `-`).
def sub [m] (u: [m]u64) (v: [m]u64) : ([m]u64, bool) =
  bsub64 u v

-- Quotient of big integer `u` by one digit `d` (i.e. `u quo d`).
def quod [m] (u: [m]u64) (d: u64) : [m]u64 =
  u -- TODO this can be done using a scan (I think) - algorithms seems to exist

-- Multiplication of big integers `u` and `v`.
def mul [m] (u: [m]u64) (v: [m]u64) : [m]u64 =
  convMult64v1 u v

-- Multiplication of big integer `u` by one digit `d` (i.e. `u * d`).
def muld [m] (u: [m]u64) (d: u64) : [m]u64 =
  let (ls, hs) = unzip <| map (\ x -> (x * d, u64.mul_hi x d)) u
  let hs = map (\ i -> if i == 0 then 0 else hs[i-1]) (iota m)
  in add ls hs

-- Computes `(u * v) rem ((2^64)^e)` of big integers `u`, `v` and exponent `e`.
def multmod [m] (v: [m]u64) (w: [m]u64) (e: i64) : [m]u64 =
  let retval = mul (take e v) (take e w)
  in map (\ i -> if i < e then retval[i] else 0u64) (iota m)

-- Computes `(2^64)^h - (v * w)` with sign (false is `+` and true is `-`).
def powdiff [m] (v: [m]u64) (w: [m]u64) (h: i64) (l: i64) : ([m]u64, bool) =
  let L = (prec v) + (prec w) - l + 1
  in if (ez64 v) || (ez64 w) then (bpow m h, false)
     else if L >= h then sub (bpow m h) (mul v w)
     else let P = multmod v w L
          in if ez64 P then (P, false)
             else if P[L-1] == 0 then (P, true)
             else sub (bpow m L) P

-- Takes an iterative refinement step (see paper for details).
def step [m] (v: [m]u64) (w: [m]u64) (h: i64) (n: i64) (l: i64) (g:i64): [m]u64=
  let (pd, sign) = powdiff v w (h-n) (l-g)
  let pdw_s = mul pd w |> shift (2*n-h)
  let w_s = shift n w
  in if sign then (sub pdw_s w_s |> fst) else add pdw_s w_s

-- Refinement strategy 1 (see paper for details); DOES NOT WORK, USE ANOTHER.
def refine1 [m] (v: [m]u64) (w: [m]u64) (h: i64) (k: i64) (l: i64) : [m]u64 =
  let g = 1
  let h = h + g
  let (w, _) = -- scale initial value to full length
    loop (w, l) = (shift (h-k-l) w, l) while h - k > l do
    let w = step v w h 0 l 0
    let l = i64.min (2*l-1) (h-k) -- number of correct digits
    in (w, l)
  in shift (-g) w

-- Refinement strategy 2 (see paper for details).
def refine2 [m] (v: [m]u64) (w: [m]u64) (h: i64) (k: i64) (l: i64) : [m]u64 =
  let g = m / 4 -- `m` guard digits; NOTE `m/4` because padding it factor '4'
  let (w, _) =
    loop (w, l) = (shift g w, l) while h - k > l do
    let n = i64.min l (h-k+1-l) -- how much to grow
    let w = shift (-1) <| step v w (k+l+n+g) n l g
    let l = l + n - 1 -- number of correct digits
    in (w, l)
  in shift (-g) w

-- Refinement strategy 3 (see paper for details).
def refine3 [m] (v: [m]u64) (w: [m]u64) (h: i64) (k: i64) (l: i64) : [m]u64 =
  let g = m / 4 -- `m` guard digits; NOTE `m/4` because padding is factor '4'
  let (w, _) =
    loop (w, l) = (shift g w, l) while h - k > l do
    let n = i64.min l (h-k+1-l) -- how much to grow
    let s = i64.max 0 (k-2*l+1-g) -- how to scale v
    let w = shift (-1) <| step (shift (-s) v) w (k+l+n-s+g) n l g
    let l = l + n - 1 -- number of correct digits
    in (w, l)
  in shift (-g) w

-- Whole shifted inverse of big integer `v` by `h`.
def shinv [m] (k: i64) (v: [m]u64) (h: i64) : [m]u64 =
  -- handle the four special cases
  if ltBpow v 1 then quod (bpow m h) v[0] -- TODO reducable to 'k==0'
  else if gtBpow v h then new64 m
  else if gtBpow (muld v 2) h then singleton64 m 1
  else if eqBpow v k then bpow m (h - k)
  -- form initial approximation
  else let l = 2
       let V = [v[k-2], v[k-1], v[k]]
       let w = new64 m -- TODO compute w as `(B^4 - V) / V + 1`
       -- either return or refine initial approximation
       in if h - k <= l then shift (h - k - l) w
          else refine3 v w h k l

-- Divides big integer `u` by big integer `v`.
def div [m] (u: [m]u64) (v: [m]u64) : ([m]u64, [m]u64) =
  -- compute `h` and `k` in the assumptions `u <= B^h` and 'B^k <= v < B^(k+1)'
  let h = findh u
  let k = findk v
  -- pad big integers in advance for multiplications (excessive)
  let u_p = map (\ i -> if i < m then u[i] else 0u64) (iota (m*4))
  let v_p = map (\ i -> if i < m then v[i] else 0u64) (iota (m*4))
  -- if `k = 1`, we shift by one to fit the algorithm
  let (h, k, u_p, v_p) = if k == 1 then (h+1, k+1, shift 1 u_p, shift 1 v_p)
                         else (h, k, u_p, v_p)
  -- compute quotient using Theorem 1. of the paper
  let q = shinv k v_p h |> mul u_p |> shift (-h) |> take m
  -- compute remainder
  let r = mul v q |> sub u |> fst
  -- handle delta (i.e. if `r >= v` then `delta = 1` else `delta = 0`
  in if not (lt64 r v) then (add q (singleton64 m 1), fst (sub r v)) else (q, r)
