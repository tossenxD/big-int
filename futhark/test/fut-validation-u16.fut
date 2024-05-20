import "fut-validation-main"

-- Run test entries with base `u16` 1-dimensional random big integers.
-- ==
-- entry: add1D16 mul1D16
-- compiled random input { [4]u16 [4]u16 }
-- output { true }
-- compiled random input { [8]u16 [8]u16 }
-- output { true }
-- compiled random input { [16]u16 [16]u16 }
-- output { true }
-- compiled random input { [32]u16 [32]u16 }
-- output { true }
-- compiled random input { [64]u16 [64]u16 }
-- output { true }
-- compiled random input { [128]u16 [128]u16 }
-- output { true }
-- compiled random input { [256]u16 [256]u16 }
-- output { true }
-- compiled random input { [512]u16 [512]u16 }
-- output { true }

-- Run test entries with base `u16` 2-dimensional random big integers.
-- ==
-- entry: add2D16 mul2D16
-- compiled random input { [2048][4]u16 [2048][4]u16 }
-- output { true }
-- compiled random input { [2048][8]u16 [2048][8]u16 }
-- output { true }
-- compiled random input { [2048][16]u16 [2048][16]u16 }
-- output { true }
-- compiled random input { [2048][32]u16 [2048][32]u16 }
-- output { true }
-- compiled random input { [2048][64]u16 [2048][64]u16 }
-- output { true }
-- compiled random input { [2048][128]u16 [2048][128]u16 }
-- output { true }

entry add1D16 [m] (u: [m]u16) (v: [m]u16) : bool =
  add1D u v

entry add2D16 [n][m] (us: [n][m]u16) (vs: [n][m]u16) : bool =
  add2D us vs

entry mul1D16 [m] (u: [m]u16) (v: [m]u16) : bool =
  mul1D u v

entry mul2D16 [n][m] (us: [n][m]u16) (vs: [n][m]u16) : bool =
  mul2D us vs
