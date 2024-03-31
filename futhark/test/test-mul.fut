import "../mul"

-- ==
-- entry: test_mul32
-- input { [1u32, 2u32] [3u32, 4u32] }
-- output { [3u32, 8u32] }
entry test_mul32 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  convMult32 as bs

-- ==
-- entry: test_mul64
-- input { [1u64, 2u64] [3u64, 4u64] }
-- output { [3u64, 8u64] }
entry test_mul64 [n] (as: [n]u64) (bs: [n]u64) : [n]u64 =
  convMult64 as bs
