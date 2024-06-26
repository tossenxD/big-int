import "fut-properties-main"

-- Run tests taking one random base `u64` big integer.
-- ==
-- entry: addIdentity64 mulIdentity64 mulZero64
-- compiled random input { [128][4]u64 }
-- output { true }
-- compiled random input { [128][16]u64 }
-- output { true }
-- compiled random input { [128][64]u64 }
-- output { true }
-- compiled random input { [128][128]u64 }
-- output { true }

-- Run test taking two random base `u64` big integers.
-- ==
-- entry: addCommutative64 mulCommutative64
-- compiled random input { [128][4]u64 [128][4]u64 }
-- output { true }
-- compiled random input { [128][16]u64 [128][16]u64 }
-- output { true }
-- compiled random input { [128][64]u64 [128][64]u64 }
-- output { true }
-- compiled random input { [128][128]u64 [128][128]u64 }
-- output { true }

-- Run test taking three random base `u64` big integers.
-- ==
-- entry: addAssociative64 mulAssociative64 mulDistributive64
-- compiled random input { [128][4]u64 [128][4]u64 [128][4]u64 }
-- output { true }
-- compiled random input { [128][16]u64 [128][16]u64 [128][16]u64 }
-- output { true }
-- compiled random input { [128][64]u64 [128][64]u64 [128][64]u64 }
-- output { true }
-- compiled random input { [128][128]u64 [128][128]u64 [128][128]u64 }
-- output { true }

entry addIdentity64 [n][m] (us: [n][m]u64) : bool =
  map addIdentity us |> reduce (&&) true

entry addCommutative64 [n][m] (us: [n][m]u64) (vs: [n][m]u64) : bool =
  map2 addCommutative us vs |> reduce (&&) true

entry addAssociative64
[n][m] (ss: [n][m]u64) (us: [n][m]u64) (vs: [n][m]u64) : bool =
  map3 addAssociative ss us vs |> reduce (&&) true

entry mulIdentity64 [n][m] (us: [n][m]u64) : bool =
  map mulIdentity us |> reduce (&&) true

entry mulZero64 [n][m] (us: [n][m]u64) : bool =
  map mulZero us |> reduce (&&) true

entry mulCommutative64
[n][m] (us: [n][m]u64) (vs: [n][m]u64) : bool =
  map2 mulCommutative us vs |> reduce (&&) true

entry mulAssociative64
[n][m] (ss: [n][m]u64) (us: [n][m]u64) (vs: [n][m]u64) : bool =
  map3 mulAssociative ss us vs |> reduce (&&) true

entry mulDistributive64
[n][m] (ss: [n][m]u64) (us: [n][m]u64) (vs: [n][m]u64) : bool =
  map3 mulDistributive ss us vs |> reduce (&&) true
