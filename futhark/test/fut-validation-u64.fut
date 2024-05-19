import "fut-validation-main"

-- Run test entries with base `u64` 1-dimensional random big integers.
-- ==
-- entry: add1D64 mul1D64
-- compiled random input { [4]u64 [4]u64 }
-- output { true }
-- compiled random input { [8]u64 [8]u64 }
-- output { true }
-- compiled random input { [16]u64 [16]u64 }
-- output { true }
-- compiled random input { [32]u64 [32]u64 }
-- output { true }
-- compiled random input { [64]u64 [64]u64 }
-- output { true }
-- compiled random input { [128]u64 [128]u64 }
-- output { true }
-- compiled random input { [256]u64 [256]u64 }
-- output { true }
-- compiled random input { [512]u64 [512]u64 }
-- output { true }

-- Run test entries with base `u64` 2-dimensional random big integers.
-- ==
-- entry: add2D64 mul2D64
-- compiled random input { [2048][4]u64 [2048][4]u64 }
-- output { true }
-- compiled random input { [2048][8]u64 [2048][8]u64 }
-- output { true }
-- compiled random input { [2048][16]u64 [2048][16]u64 }
-- output { true }
-- compiled random input { [2048][32]u64 [2048][32]u64 }
-- output { true }
-- compiled random input { [2048][64]u64 [2048][64]u64 }
-- output { true }
-- compiled random input { [2048][128]u64 [2048][128]u64 }
-- output { true }

entry add1D64 [m] (u: [m]u64) (v: [m]u64) : bool =
  add1D u v

entry add2D64 [n][m] (us: [n][m]u64) (vs: [n][m]u64) : bool =
  add2D us vs

entry mul1D64 [m] (u: [m]u64) (v: [m]u64) : bool =
  mul1D u v

entry mul2D64 [n][m] (us: [n][m]u64) (vs: [n][m]u64) : bool =
  mul2D us vs
