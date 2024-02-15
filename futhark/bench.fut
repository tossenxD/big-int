import "add"

-- ==
-- entry: oneAdd0
-- compiled random input { [10000][1000]u32   [10000][1000]u32   }
-- compiled random input { [10000][10000]u32  [10000][10000]u32  }
-- compiled random input { [10000][50000]u32  [10000][50000]u32  }
entry oneAdd0 [m][n] (ass: [m][n]u32) (bss: [m][n]u32) : [m][n]u32 =
  map2 add32 ass bss

-- ==
-- entry: oneAdd1
-- compiled random input { [10000][1000]u32   [10000][1000]u32   }
-- compiled random input { [10000][10000]u32  [10000][10000]u32  }
-- compiled random input { [10000][50000]u32  [10000][50000]u32  }
entry oneAdd1 [m][n] (ass: [m][n]u32) (bss: [m][n]u32) : [m][n]u32 =
  map2 badd ass bss

-- ==
-- entry: oneAdd2
-- compiled random input { [10000][1000]u32   [10000][1000]u32   }
-- compiled random input { [10000][10000]u32  [10000][10000]u32  }
-- compiled random input { [10000][50000]u32  [10000][50000]u32  }
entry oneAdd2 [m][n] (ass: [m][n]u32) (bss: [m][n]u32) : [m][n]u32 =
  let as = flatten ass :> [m*(4*(n/4))]u32
  let bs = flatten bss :> [m*(4*(n/4))]u32
  in badd_opt as bs |> unflatten :> [m][n]u32
