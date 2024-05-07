-- Functions

def imap as g = map g as

def imap2 as bs g = map2 g as bs

let imap2Intra as bs f = #[incremental_flattening(only_intra)] map2 f as bs

def imap3 as bs cs g = map3 g as bs cs

def scan_exc [n] 'a (g: (a -> a -> a)) (ne: a)  (xs: [n]a) : [n]a =
  let xs = scan g ne xs
  in map (\ i -> if i == 0 then ne else xs[i-1]) (0..<n)

def seg_scan_exc [n] 'a (g: a->a->a) (ne: a) (fs: [n]bool) (xs: [n]a) : [n]a =
  let (res, _) = scan ( \ (v1, f1) (v2, f2) ->
                          let f = f1 || f2
                          let v = if f2 then v2 else g v1 v2
                          in (v,f)
                      ) (ne, false) (zip xs fs) |> unzip
  in map2 (\ i f -> if f then ne else res[i-1]) (0..<n) fs

def fst 'a 'b (tp: (a, b)) : a =
  tp.0

-- Big Integers

def new (m: i64) : [m]u64 =
  replicate m 0u64

def singleton (m: i64) (d: u64) : [m]u64 =
  map (\ i -> if i == 0 then d else 0u64) (iota m)

def lt [m] (u: [m]u64) (v: [m]u64) : bool =
  map2 (\ x y -> (x < y, x == y)) u v
  |> reduce (\ (accl, _) (l, e) -> (l || (e && accl), true)) (false, true)
  |> fst

def eq [m] 't (f: t -> t -> bool) (u: [m]t) (v: [m]t) : bool =
  map2 f u v |> reduce (&&) true

def ez [m] (u: [m]u64) : bool =
  map (0 ==) u |> reduce (&&) true

def pad1d [m] 't (a: i64) (e: t) (u: [m]t) : []t =
  let p = (a - (m % a)) % a
  in u ++ replicate p e
