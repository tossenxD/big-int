import "fut-properties-main"

-- Run tests taking one random base `u16` big integer.
-- ==
-- entry: addIdentity16 mulIdentity16 mulZero16
-- compiled random input { [128][4]u16 }
-- output { true }
-- compiled random input { [128][16]u16 }
-- output { true }
-- compiled random input { [128][64]u16 }
-- output { true }
-- compiled random input { [128][128]u16 }
-- output { true }

-- Run test taking two random base `u16` big integers.
-- ==
-- entry: addCommutative16 mulCommutative16
-- compiled random input { [128][4]u16 [128][4]u16 }
-- output { true }
-- compiled random input { [128][16]u16 [128][16]u16 }
-- output { true }
-- compiled random input { [128][64]u16 [128][64]u16 }
-- output { true }
-- compiled random input { [128][128]u16 [128][128]u16 }
-- output { true }

-- Run test taking three random base `u16` big integers.
-- ==
-- entry: addAssociative16 mulAssociative16 mulDistributive16
-- compiled random input { [128][4]u16 [128][4]u16 [128][4]u16 }
-- output { true }
-- compiled random input { [128][16]u16 [128][16]u16 [128][16]u16 }
-- output { true }
-- compiled random input { [128][64]u16 [128][64]u16 [128][64]u16 }
-- output { true }
-- compiled random input { [128][128]u16 [128][128]u16 [128][128]u16 }
-- output { true }

entry addIdentity16 [n][m] (us: [n][m]u16) : bool =
  map addIdentity us |> reduce (&&) true

entry addCommutative16 [n][m] (us: [n][m]u16) (vs: [n][m]u16) : bool =
  map2 addCommutative us vs |> reduce (&&) true

entry addAssociative16
[n][m] (ss: [n][m]u16) (us: [n][m]u16) (vs: [n][m]u16) : bool =
  map3 addAssociative ss us vs |> reduce (&&) true

entry mulIdentity16 [n][m] (us: [n][m]u16) : bool =
  map mulIdentity us |> reduce (&&) true

entry mulZero16 [n][m] (us: [n][m]u16) : bool =
  map mulZero us |> reduce (&&) true

entry mulCommutative16
[n][m] (us: [n][m]u16) (vs: [n][m]u16) : bool =
  map2 mulCommutative us vs |> reduce (&&) true

entry mulAssociative16
[n][m] (ss: [n][m]u16) (us: [n][m]u16) (vs: [n][m]u16) : bool =
  map3 mulAssociative ss us vs |> reduce (&&) true

entry mulDistributive16
[n][m] (ss: [n][m]u16) (us: [n][m]u16) (vs: [n][m]u16) : bool =
  map3 mulDistributive ss us vs |> reduce (&&) true
