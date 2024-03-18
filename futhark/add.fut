import "helper"

--------------------------------------------------------------------------------
-- operators / helper functions
--------------------------------------------------------------------------------

-- logical associative operator for addition
def add_op (ov1: bool, mx1: bool) (ov2: bool, mx2: bool) : (bool, bool) =
  ((ov1 && mx2) || ov2, mx1 && mx2)

-- bitwise version; ov = `1`, mx = `2` and neutral element = 2
def carry_prop32 (c1: u32) (c2: u32) : u32 =
  (c1 & c2 & 2) | (((c1 & (c2 >> 1)) | c2) & 1)

def carry_prop64 (c1: u64) (c2: u64) : u64 =
  (c1 & c2 & 2) | (((c1 & (c2 >> 1)) | c2) & 1)

-- computes the un-propagated bitwise carry for a unit
def initc32 (r : u32) (a : u32) : u32 =
  (u32.bool (r < a)) | ((u32.bool (r == u32.highest)) << 1)

def initc64 (r : u64) (a : u64) : u64 =
  (u64.bool (r < a)) | ((u64.bool (r == u64.highest)) << 1)

                                                          
--------------------------------------------------------------------------------
-- approach outlined by students as part of the DPP course
--------------------------------------------------------------------------------

def add32 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  -- 1. compute result and carry
  let (res, cs) = map2 (\ a b -> let r = a + b
                                 in (r, (r < a, r == u32.highest))
                       ) as bs |> unzip

  -- 2. propagate carries
  let (carries, _) = scan_exc add_op (false, true) cs |> unzip

  -- add carries to result
  in imap2 res carries (\ r c -> r + u32.bool c)

def add64 [n] (as: [n]u64) (bs: [n]u64) : [n]u64 =
  -- 1. compute result and carry
  let (res, cs) = map2 (\ a b -> let r = a + b
                                 in (r, (r < a, r == u64.highest))
                       ) as bs |> unzip

  -- 2. propagate carries
  let (carries, _) = scan_exc add_op (false, true) cs |> unzip

  -- add carries to result
  in imap2 res carries (\ r c -> r + u64.bool c)


--------------------------------------------------------------------------------
-- optimized approach using bitwise instead of logical operators
--------------------------------------------------------------------------------

def badd32 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  -- 1. compute result and carry
  let (res, cs) = map2 (\ a b -> let r = a + b in (r,initc32 r a)) as bs |>unzip

  -- 2. propagate carries
  let carries = scan_exc carry_prop32 2 cs
  
  -- 3. add carries to result
  in imap2 res carries (\ r c -> r + (c & 1))

def badd64 [n] (as: [n]u64) (bs: [n]u64) : [n]u64 =
  -- 1. compute result and carry
  let (res, cs) = map2 (\ a b -> let r = a + b in (r,initc64 r a)) as bs |>unzip

  -- 2. propagate carries
  let carries = scan_exc carry_prop64 2 cs
  
  -- 3. add carries to result
  in imap2 res carries (\ r c -> r + (c & 1))
