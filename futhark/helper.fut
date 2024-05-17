--------------------------------------------------------------------------------
-- Data Structures
--------------------------------------------------------------------------------

type ui = u64
type ct = u32

def HIGHEST  : ui               = u64.highest
def bits     : i64              = 64
def fromBool : (bool -> ui)     = u64.bool
def boolToCt : (bool -> ct)     = u32.bool
def fromCt   : (ct -> ui)       = u64.u32
def mulHigh  : (ui -> ui -> ui) = u64.mul_hi

-- type ui = u32
-- type ct = u32

-- def HIGHEST  : ui            = u32.highest
-- def bits     : i64           = 32
-- def fromBool : (bool -> ui)  = u32.bool
-- def boolToCt : (bool -> ct)  = u32.bool
-- def fromCt   : (ct -> ui)    = u32.u32
-- def mulHigh  : (ui -> ui)    = u32.mul_hi




-- Functions

def imap as g = map g as

def imap2 as bs g = map2 g as bs

let imap2Intra as bs f = #[incremental_flattening(only_intra)] map2 f as bs

def imap3 as bs cs g = map3 g as bs cs

def fst 'a 'b (tp: (a, b)) : a =
  tp.0

-- Big Integers

def new64 (m: i64) : [m]u64 =
  replicate m 0u64

def singleton64 (m: i64) (d: u64) : [m]u64 =
  map (\ i -> if i == 0 then d else 0u64) (iota m)

def lt64 [m] (u: [m]u64) (v: [m]u64) : bool =
  map2 (\ x y -> (x < y, x == y)) u v
  |> reduce (\ (accl, _) (l, e) -> (l || (e && accl), true)) (false, true)
  |> fst

def lt32 [m] (u: [m]u32) (v: [m]u32) : bool =
  map2 (\ x y -> (x < y, x == y)) u v
  |> reduce (\ (accl, _) (l, e) -> (l || (e && accl), true)) (false, true)
  |> fst

def eq [m] 't (f: t -> t -> bool) (u: [m]t) (v: [m]t) : bool =
  map2 f u v |> reduce (&&) true

def ez64 [m] (u: [m]u64) : bool =
  map (0 ==) u |> reduce (&&) true

def pad1d [m] 't (a: i64) (e: t) (u: [m]t) : []t =
  let p = (a - (m % a)) % a
  in u ++ replicate p e

  -- let p = (4 - (n % 4)) % 4
  -- let pz = map (\_ -> 0) (0..<p)
  -- let as = as ++ pz :> [4*((n + p) / 4)]u32
  -- let bs = bs ++ pz :> [4*((n + p) / 4)]u32
  -- in take n <| badd32v2Run as bs
