import "../mul"

-- ==
-- entry: test_mul32v1
-- input { [1u32, 2u32, 3u32, 4u32] [5u32, 6u32, 7u32, 8u32] }
-- output { [5u32, 16u32, 34u32, 60u32] }
-- input { [12u32, 23u32, 34u32, 45u32] [56u32, 67u32, 78u32, 89u32] }
-- output { [672u32, 2092u32, 4381u32, 7660u32] }
entry test_mul32v1 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  convMult32v1 as bs

-- ==
-- entry: test_mul64v1
-- input { [1u64, 2u64, 3u64, 4u64] [5u64, 6u64, 7u64, 8u64] }
-- output { [5u64, 16u64, 34u64, 60u64] }
-- input { [12u64, 23u64, 34u64, 45u64] [56u64, 67u64, 78u64, 89u64] }
-- output { [672u64, 2092u64, 4381u64, 7660u64] }
entry test_mul64v1 [n] (as: [n]u64) (bs: [n]u64) : [n]u64 =
  convMult64v1 as bs

-- ==
-- entry: test_mul64v2
-- input { [1u64, 2u64, 3u64, 4u64] [5u64, 6u64, 7u64, 8u64] }
-- output { [5u64, 16u64, 34u64, 60u64] }
-- input { [12u64, 23u64, 34u64, 45u64] [56u64, 67u64, 78u64, 89u64] }
-- output { [672u64, 2092u64, 4381u64, 7660u64] }
entry test_mul64v2 [n] (as: [n]u64) (bs: [n]u64) : [n]u64 =
  convMult64v2 as bs
