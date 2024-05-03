import "gmp-validation-lib"
import "../helper"
import "../add"
import "../mul"
import "../div"

-- ==
-- entry: add32_1d
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
entry add32_1d [m] (u: [m]u32) (v: [m]u32) : bool =
  let validP = eq (u32.==) (test_add32 u v)
  let w0 = add32 u v
  let w1 = badd32v1 u v
  let w2 = badd32v2 u v
  let w3 = badd32v3 1 u v
  in validP w0 && validP w1 && validP w2 && validP w3

-- ==
-- entry: add64_1d
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
entry add64_1d [m] (u: [m]u64) (v: [m]u64) : bool =
  let validP = eq (u64.==) (test_add64 u v)
  let w0 = add64 u v
  let w1 = badd64v1 u v
  let w2 = badd64v2 u v
  let w3 = badd64v3 1 u v
  in validP w0 && validP w1 && validP w2 && validP w3

-- ==
-- entry: add32_2d
-- compiled random input { [2048][4]u32 [2048][4]u32 }
-- output { true }
-- compiled random input { [2048][16]u32 [2048][16]u32 }
-- output { true }
-- compiled random input { [2048][64]u32 [2048][64]u32 }
-- output { true }
-- compiled random input { [2048][512]u32 [2048][512]u32 }
-- output { true }
-- compiled random input { [4441][1]u32 [4441][1]u32 }
-- output { true }
-- compiled random input { [4441][37]u32 [4441][37]u32 }
-- output { true }
-- compiled random input { [4441][113]u32 [4441][113]u32 }
-- output { true }
-- compiled random input { [4441][443]u32 [4441][443]u32 }
-- output { true }
entry add32_2d [n][m] (us: [n][m]u32) (vs: [n][m]u32) : bool =
  let validP =
    (\ ws -> map2 (eq (u32.==)) (map2 test_add32 us vs) ws |> reduce (&&) true)
  let ws0 = map2 add32 us vs
  let ws1 = map2 badd32v1 us vs
  let ws2 = map2 badd32v2 us vs
  let ws3 = badd32v3Wrapper us vs
  in validP ws0 && validP ws1 && validP ws2 && validP ws3

-- ==
-- entry: add64_2d
-- compiled random input { [2048][4]u64 [2048][4]u64 }
-- output { true }
-- compiled random input { [2048][16]u64 [2048][16]u64 }
-- output { true }
-- compiled random input { [2048][64]u64 [2048][64]u64 }
-- output { true }
-- compiled random input { [2048][512]u64 [2048][512]u64 }
-- output { true }
-- compiled random input { [4441][1]u64 [4441][1]u64 }
-- output { true }
-- compiled random input { [4441][37]u64 [4441][37]u64 }
-- output { true }
-- compiled random input { [4441][113]u64 [4441][113]u64 }
-- output { true }
-- compiled random input { [4441][443]u64 [4441][443]u64 }
-- output { true }
entry add64_2d [n][m] (us: [n][m]u64) (vs: [n][m]u64) : bool =
  let validP =
    (\ ws -> map2 (eq (u64.==)) (map2 test_add64 us vs) ws |> reduce (&&) true)
  let ws0 = map2 add64 us vs
  let ws1 = map2 badd64v1 us vs
  let ws2 = map2 badd64v2 us vs
  let ws3 = badd64v3Wrapper us vs
  in validP ws0 && validP ws1 && validP ws2 && validP ws3
