def imap as g = map g as

def imap2 as bs g = map2 g as bs

def imap3 as bs cs g = map3 g as bs cs

def scan_exc [n] 'a (g: (a -> a -> a)) (ne: a)  (xs: [n]a) : [n]a =
  let xs = scan g ne xs
  in map (\ i -> if i == 0 then ne else xs[i-1]) (0..<n)

def seg_scan_exc [n] 'a (g: a -> a -> a) (ne: a) (fs: [n]bool) (xs: [n]a) : [n]a =
  let (res, _) = scan ( \ (v1, f1) (v2, f2) ->
                          let f = f1 || f2
                          let v = if f2 then v2 else g v1 v2
                          in (v,f)
                      ) (ne, false) (zip xs fs) |> unzip
  in map2 (\ i f -> if f then ne else res[i-1]) (0..<n) fs

def rev [n] 'a (as: [n]a) : [n]a =
  map (\ i -> as[n-1 - i]) (0..<n)
