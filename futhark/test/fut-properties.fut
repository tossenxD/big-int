import "gmp-validation-lib"
import "../helper"
import "../add"
import "../mul"
import "../div"

-- ==
-- entry: add32_commutative
-- compiled random input { [4]u32 [4]u32 }
-- output { true }
-- compiled random input { [16]u32 [16]u32 }
-- output { true }
-- compiled random input { [64]u32 [64]u32 }
-- output { true }
-- compiled random input { [512]u32 [512]u32 }
-- output { true }
-- compiled random input { [1]u32 [1]u32 }
-- output { true }
-- compiled random input { [37]u32 [37]u32 }
-- output { true }
-- compiled random input { [113]u32 [113]u32 }
-- output { true }
-- compiled random input { [443]u32 [443]u32 }
-- output { true }
entry add32_commutative [m] (u: [m]u32) (v: [m]u32) : bool =
  let b0 = eq (u32.==) (add32 u v) (add32 v u)
  let b1 = eq (u32.==) (badd32v1 u v) (badd32v1 v u)
  let b2 = eq (u32.==) (badd32v2 u v) (badd32v2 v u)
  let b3 = eq (u32.==) (badd32v3 1 u v) (badd32v3 1 v u)
  in b0 && b1 && b2 && b3

-- ==
-- entry: add64_commutative
-- compiled random input { [4]u64 [4]u64 }
-- output { true }
-- compiled random input { [16]u64 [16]u64 }
-- output { true }
-- compiled random input { [64]u64 [64]u64 }
-- output { true }
-- compiled random input { [512]u64 [512]u64 }
-- output { true }
-- compiled random input { [1]u64 [1]u64 }
-- output { true }
-- compiled random input { [37]u64 [37]u64 }
-- output { true }
-- compiled random input { [113]u64 [113]u64 }
-- output { true }
-- compiled random input { [443]u64 [443]u64 }
-- output { true }
entry add64_commutative [m] (u: [m]u64) (v: [m]u64) : bool =
  let b0 = eq (u64.==) (add64 u v) (add64 v u)
  let b1 = eq (u64.==) (badd64v1 u v) (badd64v1 v u)
  let b2 = eq (u64.==) (badd64v2 u v) (badd64v2 v u)
  let b3 = eq (u64.==) (badd64v3 1 u v) (badd64v3 1 v u)
  in b0 && b1 && b2 && b3

-- ==
-- entry: add32_associative
-- compiled random input { [4]u32 [4]u32 [4]u32 }
-- output { true }
-- compiled random input { [16]u32 [16]u32 [16]u32 }
-- output { true }
-- compiled random input { [64]u32 [64]u32 [64]u32 }
-- output { true }
-- compiled random input { [512]u32 [512]u32 [512]u32 }
-- output { true }
-- compiled random input { [1]u32 [1]u32 [1]u32 }
-- output { true }
-- compiled random input { [37]u32 [37]u32 [37]u32 }
-- output { true }
-- compiled random input { [113]u32 [113]u32 [113]u32 }
-- output { true }
-- compiled random input { [443]u32 [443]u32 [443]u32 }
-- output { true }
entry add32_associative [m] (s: [m]u32) (u: [m]u32) (v: [m]u32) : bool =
  let b0 = eq (u32.==) (add32 (add32 s u) v) (add32 s (add32 u v))
  let b1 = eq (u32.==) (badd32v1 (badd32v1 s u) v) (badd32v1 s (badd32v1 u v))
  let b2 = eq (u32.==) (badd32v2 (badd32v2 s u) v) (badd32v2 s (badd32v2 u v))
  let b3 = eq (u32.==) (badd32v3 1 (badd32v3 1 s u) v) (badd32v3 1 s (badd32v3 1 u v))
  in b0 && b1 && b2 && b3

-- ==
-- entry: add64_associative
-- compiled random input { [4]u64 [4]u64 [4]u64 }
-- output { true }
-- compiled random input { [16]u64 [16]u64 [16]u64 }
-- output { true }
-- compiled random input { [64]u64 [64]u64 [64]u64 }
-- output { true }
-- compiled random input { [512]u64 [512]u64 [512]u64 }
-- output { true }
-- compiled random input { [1]u64 [1]u64 [1]u64 }
-- output { true }
-- compiled random input { [37]u64 [37]u64 [37]u64 }
-- output { true }
-- compiled random input { [113]u64 [113]u64 [113]u64 }
-- output { true }
-- compiled random input { [443]u64 [443]u64 [443]u64 }
-- output { true }
entry add64_associative [m] (s: [m]u64) (u: [m]u64) (v: [m]u64) : bool =
  let b0 = eq (u64.==) (add64 (add64 s u) v) (add64 s (add64 u v))
  let b1 = eq (u64.==) (badd64v1 (badd64v1 s u) v) (badd64v1 s (badd64v1 u v))
  let b2 = eq (u64.==) (badd64v2 (badd64v2 s u) v) (badd64v2 s (badd64v2 u v))
  let b3 = eq (u64.==) (badd64v3 1 (badd64v3 1 s u) v) (badd64v3 1 s (badd64v3 1 u v))
  in b0 && b1 && b2 && b3
