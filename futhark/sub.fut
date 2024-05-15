import "helper"
import "add"

--------------------------------------------------------------------------------
-- based on baddv1, see `add.fut` for more info; (false is `+`, true is `-`)
--------------------------------------------------------------------------------

entry bsub32 [n] (as: [n]u32) (bs: [n]u32) : ([n]u32, bool) =
  -- function to compute (pre-scaned) augmented carries
  let initc (r : u32) (a : u32) : u32 =
    (u32.bool (r > a)) | ((u32.bool (r == 0)) << 1)

  -- 1. compute sign
  let (as, bs, sign) = if lt32 as bs then (bs, as, true) else (as, bs, false)

  -- 2. compute result and carries
  let (res, cs) = map2 (\ a b -> let r = a - b in (r, initc r a)) as bs |> unzip

  -- 3. propagate carries
  let carries = scan_exc carry_prop32 2 cs

  -- 4. subtract carries from result
  in (imap2 res carries (\ r c -> r - (c & 1)), sign)

entry bsub64 [n] (as: [n]u64) (bs: [n]u64) : ([n]u64, bool) =
  -- function to compute (pre-scanned) augmented carries
  let initc (r : u64) (a : u64) : u64 =
    (u64.bool (r > a)) | ((u64.bool (r == 0)) << 1)

  -- 1. compute sign
  let (as, bs, sign) = if lt64 as bs then (bs, as, true) else (as, bs, false)
  
  -- 2. compute result and carry
  let (res, cs) = map2 (\ a b -> let r = a - b in (r, initc r a)) as bs |> unzip

  -- 3. propagate carries
  let carries = scan_exc carry_prop64 2 cs

  -- 4. add carries to result
  in (imap2 res carries (\ r c -> r - (c & 1)), sign)
