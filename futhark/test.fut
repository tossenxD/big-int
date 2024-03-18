import "add"

--------------------------------------------------------------------------------
-- addition
--------------------------------------------------------------------------------

-- ==
-- entry: test_add32
-- input { [1u32, 2u32] [3u32, 4u32] }
-- output { [4u32, 6u32] }
-- input { [4294967295u32, 4294967295u32, 0u32] [1u32, 0u32, 0u32] }
-- output { [0u32, 0u32, 1u32] }
-- input { [0u32, 4294967295u32] [0u32, 1u32] }
-- output { [0u32, 0u32] }
entry test_add32 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  add32 as bs

-- ==
-- entry: test_add64
-- input { [1u64, 2u64] [3u64, 4u64] }
-- output { [4u64, 6u64] }
-- input { [18446744073709551615u64, 18446744073709551615u64, 0u64]
--         [1u64,                    0u64,                    0u64] }
-- output { [0u64, 0u64, 1u64] }
-- input { [0u64, 18446744073709551615u64] [0u64, 1u64] }
-- output { [0u64, 0u64] }
entry test_add64 [n] (as: [n]u64) (bs: [n]u64) : [n]u64 =
  add64 as bs

-- ==
-- entry: test_badd32
-- input { [1u32, 2u32] [3u32, 4u32] }
-- output { [4u32, 6u32] }
-- input { [4294967295u32, 4294967295u32, 0u32] [1u32, 0u32, 0u32] }
-- output { [0u32, 0u32, 1u32] }
-- input { [0u32, 4294967295u32] [0u32, 1u32] }
-- output { [0u32, 0u32] }
entry test_badd32 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  badd32 as bs

-- ==
-- entry: test_badd64
-- input { [1u64, 2u64] [3u64, 4u64] }
-- output { [4u64, 6u64] }
-- input { [18446744073709551615u64, 18446744073709551615u64, 0u64]
--         [1u64,                    0u64,                    0u64] }
-- output { [0u64, 0u64, 1u64] }
-- input { [0u64, 18446744073709551615u64] [0u64, 1u64] }
-- output { [0u64, 0u64] }
entry test_badd64 [n] (as: [n]u64) (bs: [n]u64) : [n]u64 =
  badd64 as bs

--------------------------------------------------------------------------------
-- multiplication
--------------------------------------------------------------------------------

-- ==
-- entry: test_mul32
-- input { [1u32, 2u32] [3u32, 4u32] }
-- output { [3u32, 8u32] }
entry test_mul32 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  badd32 as bs

-- ==
-- entry: test_mul64
-- input { [1u64, 2u64] [3u64, 4u64] }
-- output { [3u64, 8u64] }
entry test_mul64 [n] (as: [n]u64) (bs: [n]u64) : [n]u64 =
  badd64 as bs
