import "add"

-- ==
-- entry: oneAdd0
-- input { [1u32, 2u32] [3u32, 4u32] }
-- output { [4u32, 6u32] }
-- input { [4294967295u32, 0u32] [1u32, 0u32] }
-- output { [0u32, 1u32] }
entry oneAdd0 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  add32 as bs

-- ==
-- entry: oneAdd1
-- input { [1u32, 2u32] [3u32, 4u32] }
-- output { [4u32, 6u32] }
-- input { [4294967295u32, 0u32] [1u32, 0u32] }
-- output { [0u32, 1u32] }
entry oneAdd1 [n] (as: [n]u32) (bs: [n]u32) : [n]u32 =
  badd1 as bs
