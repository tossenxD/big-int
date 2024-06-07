import "helper"
import "add"
import "sub"
import "mul"

--------------------------------------------------------------------------------
--- Big Integer Division
--------------------------------------------------------------------------------
--- The implementation is based on Algorithm 1 of the paper: ``Efficient
--- Generic Quotients Using Exact Arithmetic'' by Stephen M. Watt, 2023,
--- published: https://arxiv.org/abs/2304.01753, [1].
--- The main comments and reflection lies in the prototype `div.c`.
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- Constructs `B^n` as a big integer of size `m`.
def bpow (m: i64) (n: i64) : [m]ui =
  map (\ i -> if i+1 == n then 1 else 0 ) (iota m)

-- Checks whether `u < B^i`.
def ltBpow [m] (u: [m]ui) (i: i64) : bool =
  map2 (\ x j -> x == 0 || j < i ) u (iota m) |> reduce (&&) true

-- Checks whether `u > B^i`.
def gtBpow [m] (u: [m]ui) (i: i64) : bool =
  map2 (\ x j -> (x > 1 && j == i) || (x > 0 && j > i) ) u (iota m)
  |> reduce (||) false

-- Checks whether `u == B^i`.
def eqBpow [m] (u: [m]ui) (i: i64) : bool =
  map2 (\ x j -> (x == 0 || j != i) || (x == 1 || j == i) ) u (iota m)
  |> reduce (&&) true |> (&&) (m > i)

-- Index of most significant nonzero digit in `u`, i.e.  `B^k <= u < B^(k+1)`.
def findk [m] (u: [m]ui) : i64 =
  map2 (\ x i -> if x != 0 then i else 0 ) u (iota m)
  |> reduce (\ acc i -> if i != 0 then i else acc ) 0

-- Index `i` such that `u <= B^i`.
def findh [m] (u: [m]ui) : i64 =
  -- let i = findk u in if eqBpow u i then i else i + 1
  findk u + 1 -- NOTE this holds, but ignores the equality - use above if needed

-- Precision of `u` (i.e. number of digits minus leading zeroes).
def prec [m] (u: [m]ui) : i64 =
  findk u + 1

-- Addition of `u` and `v`.
def add [m] (u: [m]ui) (v: [m]ui) : [m]ui =
  baddV1 u v

-- Subtraction of `u` and `v` (false is `+` and true is `-`).
def sub [m] (u: [m]ui) (v: [m]ui) : ([m]ui, bool) =
  bsub u v

-- Division of `u` by a single digit `d`.
-- def quod [m] (u: [m]ui) (d: ui) : [m]ui =
-- NOTE this can be done using a scan (I think) - parallel algorithms seems to
-- exist. However, for simplicity, we shift `u` and `v` by `2` if `k==1`, in
-- order to avoid this case. (It is part of the sequential prototype `div.c`.)

-- Multiplication by single precision.
def muld [m] (u: [m]ui) (d: ui) : [m]ui =
  let (ls, hs) = unzip <| map (\ x -> (x * d, mulHigh x d) ) u
  let hs = map (\ i -> if i == 0 then 0 else hs[i-1] ) (iota m)
  in add ls hs

-- Multiplication of `u` and `v`.
def mul [m] (u: [m]ui) (v: [m]ui) : [m]ui =
  let p = (4 - (m % 4)) % 4 -- add padding to ensure alignment
  let pz = replicate p 0
  let up = u ++ pz :> [4*((m+p)/4)]ui
  let vp = v ++ pz :> [4*((m+p)/4)]ui
  in (convMulV2 up vp :> [m+p]ui) |> take m


--------------------------------------------------------------------------------
-- Main algorithm
--------------------------------------------------------------------------------

-- Computes `(v * w) rem (B^e)` of big integers `v` and `w` and exponent `e`.
def multmod [m] (v: [m]ui) (w: [m]ui) (e: i64) : [m]ui =
  -- corresponds the product of `v` and `w` truncated to size `e`
  let vw = mul v w
  in map (\ i -> if i < e then vw[i] else 0 ) (iota m)

-- Computes `B^h - (v * w)` with signs (false is `+` and true is `-`).
def powdiff [m] (v: [m]ui) (w: [m]ui) (h: i64) (l: i64) : ([m]ui, bool) =
  let L = (prec v) + (prec w) - l + 1
  in if (ez v) || (ez w) then (bpow m h, false)
     else if L >= h then sub (bpow m h) (mul v w)
     else let P = multmod v w L
          in if ez P then (P, false)
             else if P[L-1] == 0 then (P, true)
             else sub (bpow m L) P

-- Takes an iterative refinement step (see [1] for details).
def step [m] (v: [m]ui) (w: [m]ui) (h: i64) (n: i64) (l: i64) (g:i64): [m]ui =
  let (pd, sign) = powdiff v w (h-n) (l-g)
  let pdwS = mul pd w |> shift (2*n-h)
  let wS = shift n w
  in if sign then fst (sub pdwS wS) else add pdwS wS

-- Refinement strategy 1 (see [1] for details); DOES NOT WORK, USE ANOTHER.
def refine1 [m] (v: [m]ui) (w: [m]ui) (h: i64) (k: i64) (l: i64) : [m]ui =
  let g = 1
  let h = h + g
  let (w, _) = -- scale initial value to full length
    loop (w, l) = (shift (h-k-l) w, l) while h - k > l do
    let w = step v w h 0 l 0
    let l = i64.min (2*l-1) (h-k) -- number of correct digits
    in (w, l)
  in shift (-g) w

-- Refinement strategy 2 (see [1] for details).
def refine2 [m] (v: [m]ui) (w: [m]ui) (h: i64) (k: i64) (l: i64) : [m]ui =
  let g = m / 4 -- `m` guard digits; NOTE `m/4` because padding is factor '4'
  let (w, _) =
    loop (w, l) = (shift g w, l) while h - k > l do
    let n = i64.min l (h-k+1-l) -- how much to grow
    let w = shift (-1) <| step v w (k+l+n+g) n l g
    let l = l + n - 1 -- number of correct digits
    in (w, l)
  in shift (-g) w

-- Refinement strategy 3 (see [1] for details).
def refine3 [m] (v: [m]ui) (w: [m]ui) (h: i64) (k: i64) (l: i64) : [m]ui =
  let g = m / 4 -- `m` guard digits; NOTE `m/4` because padding is factor '4'
  let (w, _) =
    loop (w, l) = (shift g w, l) while h - k > l do
    let n = i64.min l (h-k+1-l) -- how much to grow
    let s = i64.max 0 (k-2*l+1-g) -- how to scale v
    let w = shift (-1) <| step (shift (-s) v) w (k+l+n-s+g) n l g
    let l = l + n - 1 -- number of correct digits
    in (w, l)
  in shift (-g) w

-- Whole shifted inverse of big integer `v` by coefficient `h`.
def shinv [m] (k: i64) (v: [m]ui) (h: i64) : [m]ui =
  -- handle the four special cases
  if ltBpow v 1 then new m -- incorrect, but assumed handled beforehand
  else if gtBpow v h then new m
  else if gtBpow (muld v 2) h then singleton m 1
  else if eqBpow v k then bpow m (h - k)
  -- form initial approximation
  else let l = 2
       let V = (toQi v[k-2]) + (toQi v[k-1] << 32) + (toQi v[k] << 64)
       let W = ((0 - V) / V) + 1 -- `(B^4 - V) / V + 1`
       let w = map (\ i -> fromQi (W >> i64ToQi i) ) (iota m)
       -- either return or refine initial approximation
       in if h - k <= l then shift (h - k - l) w
          else refine3 v w h k l

-- Main division function.
def div [m] (u: [m]ui) (v: [m]ui) : ([m]ui, [m]ui) =
  -- compute `h` and `k` in the assumptions `u <= B^h` and 'B^k <= v < B^(k+1)'
  let h = findh u
  let k = findk v
  -- pad big integers in advance for multiplications (excessive factor)
  let up = map (\ i -> if i < m then u[i] else 0 ) (iota (m*4))
  let vp = map (\ i -> if i < m then v[i] else 0 ) (iota (m*4))
  -- if `k <= 1`, we shift the inputs to fit the algorithm
  let (h, k, up, vp) = if k == 1 then (h+1, k+1, shift 1 up, shift 1 vp)
                       else if k == 2 then (h+2, k+2, shift 2 up, shift 2 vp)
                       else (h, k, up, vp)
  -- compute quotient using Theorem 1. in [1]
  let q = shinv k vp h |> mul up |> shift (-h) |> take m
  -- compute remainder
  let r = mul v q |> sub u |> fst
  -- handle delta (i.e. if `r >= v` then `delta = 1` else `delta = 0`
  in if not (lt r v) then (add q (singleton m 1), fst (sub r v)) else (q, r)
