import "fut-properties-main"

-- Run tests taking one random base `u32` big integer.
-- ==
-- entry: addIdentity32 mulIdentity32 mulZero32
-- compiled random input { [128][4]u32 }
-- output { true }
-- compiled random input { [128][16]u32 }
-- output { true }
-- compiled random input { [128][64]u32 }
-- output { true }
-- compiled random input { [128][128]u32 }
-- output { true }

-- Run test taking two random base `u32` big integers.
-- ==
-- entry: addCommutative32 mulCommutative32
-- compiled random input { [128][4]u32 [128][4]u32 }
-- output { true }
-- compiled random input { [128][16]u32 [128][16]u32 }
-- output { true }
-- compiled random input { [128][64]u32 [128][64]u32 }
-- output { true }
-- compiled random input { [128][128]u32 [128][128]u32 }
-- output { true }

-- Run test taking three random base `u32` big integers.
-- ==
-- entry: addAssociative32 mulAssociative32 mulDistributive32
-- compiled random input { [128][4]u32 [128][4]u32 [128][4]u32 }
-- output { true }
-- compiled random input { [128][16]u32 [128][16]u32 [128][16]u32 }
-- output { true }
-- compiled random input { [128][64]u32 [128][64]u32 [128][64]u32 }
-- output { true }
-- compiled random input { [128][128]u32 [128][128]u32 [128][128]u32 }
-- output { true }

entry addIdentity32 [n][m] (us: [n][m]u32) : bool =
  map (addIdentity (u32.==)) us |> reduce (&&) true

entry addCommutative32 [n][m] (us: [n][m]u32) (vs: [n][m]u32) : bool =
  map2 (addCommutative (u32.==)) us vs |> reduce (&&) true

entry addAssociative32
[n][m] (ss: [n][m]u32) (us: [n][m]u32) (vs: [n][m]u32) : bool =
  map3 (addAssociative (u32.==)) ss us vs |> reduce (&&) true

entry mulIdentity32 [n][m] (us: [n][m]u32) : bool =
  map (mulIdentity (u32.==)) us |> reduce (&&) true

entry mulZero32 [n][m] (us: [n][m]u32) : bool =
  map (mulZero (u32.==)) us |> reduce (&&) true

entry mulCommutative32
[n][m] (us: [n][m]u32) (vs: [n][m]u32) : bool =
  map2 (mulCommutative (u32.==)) us vs |> reduce (&&) true

entry mulAssociative32
[n][m] (ss: [n][m]u32) (us: [n][m]u32) (vs: [n][m]u32) : bool =
  map3 (mulAssociative (u32.==)) ss us vs |> reduce (&&) true

entry mulDistributive32
[n][m] (ss: [n][m]u32) (us: [n][m]u32) (vs: [n][m]u32) : bool =
  map3 (mulDistributive (u32.==)) ss us vs |> reduce (&&) true
