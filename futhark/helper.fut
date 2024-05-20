--------------------------------------------------------------------------------
-- Data Structures
--------------------------------------------------------------------------------

-- OUTCOMMENT FOR BASE `u64`

type ui = u64
type ct = u32
type qi = u64 -- DUMMY QUAD INT

def HIGHEST  : ui               = u64.highest
def bits     : i64              = 64
def fromBool : (bool -> ui)     = u64.bool
def boolToCt : (bool -> ct)     = u32.bool
def fromCt   : (ct -> ui)       = u64.u32
def mulHigh  : (ui -> ui -> ui) = u64.mul_hi
def toQi     : (ui -> qi)       = u64.u64 -- DUMMY QUAD INT
def fromQi   : (qi -> ui)       = u64.u64 -- DUMMY QUAD INT
def i64ToQi  : (i64 -> qi)      = u64.i64 -- DUMMY QUAD INT

-- OUTCOMMENT FOR BASE `u32`

-- type ui = u32
-- type ct = u32
-- type qi = u64 -- DUMMY QUAD INT

-- def HIGHEST  : ui               = u32.highest
-- def bits     : i64              = 32
-- def fromBool : (bool -> ui)     = u32.bool
-- def boolToCt : (bool -> ct)     = u32.bool
-- def fromCt   : (ct -> ui)       = u32.u32
-- def mulHigh  : (ui -> ui -> ui) = u32.mul_hi
-- def toQi     : (ui -> qi)       = u64.u32 -- DUMMY QUAD INT
-- def fromQi   : (qi -> ui)       = u32.u64 -- DUMMY QUAD INT
-- def i64ToQi  : (i64 -> qi)      = u64.i64 -- DUMMY QUAD INT

-- OUTCOMMENT FOR BASE `u16`

-- type ui = u16
-- type ct = u32
-- type qi = u64

-- def HIGHEST  : ui               = u16.highest
-- def bits     : i64              = 16
-- def fromBool : (bool -> ui)     = u16.bool
-- def boolToCt : (bool -> ct)     = u32.bool
-- def fromCt   : (ct -> ui)       = u16.u32
-- def mulHigh  : (ui -> ui -> ui) = u16.mul_hi
-- def toQi     : (ui -> qi)       = u64.u16
-- def fromQi   : (qi -> ui)       = u16.u64
-- def i64ToQi  : (i64 -> qi)      = u64.i64


--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

def imap as g = map g as

def imap2 as bs g = map2 g as bs

def imap4 as bs cs ds g = map4 g as bs cs ds

def imap2Intra as bs f = #[incremental_flattening(only_intra)] map2 f as bs

def fst 'a 'b (tp: (a, b)) : a = tp.0


--------------------------------------------------------------------------------
-- Big Integers
--------------------------------------------------------------------------------

-- constructors

def new (m: i64) : [m]ui =
  replicate m 0

def singleton (m: i64) (d: ui) : [m]ui =
  map (\ i -> if i == 0 then d else 0) (iota m)

-- comparisons

def lt [m] (u: [m]ui) (v: [m]ui) : bool =
  map2 (\ x y -> (x < y, x == y) ) u v
  |> reduce (\ (l1, e1) (l2, e2) -> (l2 || (e2 && l1), e1 && e2) ) (false, true)
  |> fst

def eq [m] (u: [m]ui) (v: [m]ui) : bool =
  map2 (==) u v |> reduce (&&) true

def ez [m] (u: [m]ui) : bool =
  map (0 ==) u |> reduce (&&) true

-- simple arithmetics

def shift [m] (n: i64) (u: [m]ui) : [m]ui =
  map (\ i -> let off = i - n -- positive for right, negative for left
              in if off >= 0 && off < m then u[off] else 0) (iota m)
