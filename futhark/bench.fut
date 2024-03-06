import "add"

-- ==
-- entry: oneAdd0
-- compiled random input { [131072][1024]u32 [131072][1024]u32 }
-- compiled random input { [262144][512]u32  [262144][512]u32  }
-- compiled random input { [524288][256]u32  [524288][256]u32  }
entry oneAdd0 [m][n] (ass: [m][n]u32) (bss: [m][n]u32) : [m][n]u32 =
  map2 add32 ass bss

-- ==
-- entry: oneAdd1
-- compiled random input { [131072][1024]u32 [131072][1024]u32 }
-- compiled random input { [262144][512]u32  [262144][512]u32  }
-- compiled random input { [524288][256]u32  [524288][256]u32  }
entry oneAdd1 [m][n] (ass: [m][n]u32) (bss: [m][n]u32) : [m][n]u32 =
  map2 badd1 ass bss

-- ==
-- entry: oneAdd2
-- compiled random input { [131072][1024]u32 [131072][1024]u32 }
-- compiled random input { [262144][512]u32  [262144][512]u32  }
-- compiled random input { [524288][256]u32  [524288][256]u32  }
entry oneAdd2 [m][n] (ass: [m][n]u32) (bss: [m][n]u32) : [m][n]u32 =
  map2 badd1 ass bss -- TODO fix
  -- let as = ass :> [m][(4*(n/4))]u32
  -- let bs = bss :> [m][(4*(n/4))]u32
  -- in map2 badd2 as bs
