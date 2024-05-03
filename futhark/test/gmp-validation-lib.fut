import "../add"
import "../mul"
import "../div"

entry test_add32 [m] (as: [m]u32) (bs: [m]u32) : [m]u32 =
  badd32v1 as bs

entry test_add64 [m] (as: [m]u64) (bs: [m]u64) : [m]u64 =
  badd64v1 as bs

entry test_mul32 [m] (as: [m]u32) (bs: [m]u32) : [m]u32 =
  convMult32v1 as bs

entry test_mul64 [m] (as: [m]u64) (bs: [m]u64) : [m]u64 =
  convMult64v1 as bs

entry test_div32 [m] (as: [m]u32) (bs: [m]u32) : [m]u32 =
  as

entry test_div64 [m] (as: [m]u64) (bs: [m]u64) : [m]u64 =
  as
