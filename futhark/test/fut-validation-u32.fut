import "fut-validation-main"

-- Run test entries with base `u32` 1-dimensional random big integers.
-- ==
-- entry: add1D32 mul1D32
-- compiled random input { [4]u32 [4]u32 }
-- output { true }
-- compiled random input { [8]u32 [8]u32 }
-- output { true }
-- compiled random input { [16]u32 [16]u32 }
-- output { true }
-- compiled random input { [32]u32 [32]u32 }
-- output { true }
-- compiled random input { [64]u32 [64]u32 }
-- output { true }
-- compiled random input { [128]u32 [128]u32 }
-- output { true }
-- compiled random input { [256]u32 [256]u32 }
-- output { true }
-- compiled random input { [512]u32 [512]u32 }
-- output { true }

-- Run test entries with base `u32` 2-dimensional random big integers.
-- ==
-- entry: add2D32 mul2D32
-- compiled random input { [2048][4]u32 [2048][4]u32 }
-- output { true }
-- compiled random input { [2048][8]u32 [2048][8]u32 }
-- output { true }
-- compiled random input { [2048][16]u32 [2048][16]u32 }
-- output { true }
-- compiled random input { [2048][32]u32 [2048][32]u32 }
-- output { true }
-- compiled random input { [2048][64]u32 [2048][64]u32 }
-- output { true }
-- compiled random input { [2048][128]u32 [2048][128]u32 }
-- output { true }

entry add1D32 [m] (u: [m]u32) (v: [m]u32) : bool =
  add1D u v

entry add2D32 [n][m] (us: [n][m]u32) (vs: [n][m]u32) : bool =
  add2D us vs

entry mul1D32 [m] (u: [m]u32) (v: [m]u32) : bool =
  mul1D u v

entry mul2D32 [n][m] (us: [n][m]u32) (vs: [n][m]u32) : bool =
  mul2D us vs
