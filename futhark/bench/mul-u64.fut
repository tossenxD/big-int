import "../mul"

-- Benchmarks with one instance per block and sequentialization factor of 1
-- ==
-- entry: oneMulV1Bench64 sixMulV1Bench64
-- compiled random input { [65536][2][512]u64  [65536][2][512]u64  }
-- compiled random input { [131072][2][256]u64 [131072][2][256]u64 }
-- compiled random input { [262144][2][128]u64 [262144][2][128]u64 }
-- compiled random input { [524288][2][64]u64  [524288][2][64]u64  }
-- compiled random input { [1048576][2][32]u64 [1048576][2][32]u64 }
-- compiled random input { [2097152][2][16]u64 [2097152][2][16]u64 }
-- compiled random input { [4194304][2][8]u64  [4194304][2][8]u64  }
-- compiled random input { [8388608][2][4]u64  [8388608][2][4]u64  }

-- Benchmarks with one instance per block and sequentialization factor of 4
-- ==
-- entry: oneMulV2Bench64 sixMulV2Bench64
-- compiled random input { [65536][4][256]u64  [65536][4][256]u64  }
-- compiled random input { [131072][4][128]u64 [131072][4][128]u64 }
-- compiled random input { [262144][4][64]u64  [262144][4][64]u64  }
-- compiled random input { [524288][4][32]u64  [524288][4][32]u64  }
-- compiled random input { [1048576][4][16]u64 [1048576][4][16]u64 }
-- compiled random input { [2097152][4][8]u64  [2097152][4][8]u64  }
-- compiled random input { [4194304][4][4]u64  [4194304][4][4]u64  }
-- compiled random input { [8388608][4][2]u64  [8388608][4][2]u64  }

-- Benchmarks with multiple instances per block and sequentialization factor 4
-- ==
-- entry: oneMulV3Bench64 sixMulV3Bench64
-- compiled random input { [65536][1][1024]u64 [65536][1][1024]u64 }
-- compiled random input { [131072][1][512]u64 [131072][1][512]u64 }
-- compiled random input { [262144][1][256]u64 [262144][1][256]u64 }
-- compiled random input { [262144][2][128]u64 [262144][2][128]u64 }
-- compiled random input { [262144][4][64]u64  [262144][4][64]u64  }
-- compiled random input { [262144][8][32]u64  [262144][8][32]u64  }
-- compiled random input { [262144][16][16]u64 [262144][16][16]u64 }
-- compiled random input { [262144][32][8]u64  [262144][32][8]u64  }

entry oneMulV1Bench64
[n][m] (usss: [n][2][m]u64) (vsss: [n][2][m]u64) : [n][2*m]u64 =
  -- this looks a bit wierd compared to a size-coercien, but I could not get it
  -- to fuse properly with a coercien, and it ran much slower
  let uss = map flatten usss :> [n][2*m]u64
  let vss = map flatten vsss :> [n][2*m]u64
  in oneMulV1 m uss vss

entry oneMulV2Bench64
[n][m] (usss: [n][4][m]u64) (vsss: [n][4][m]u64) : [n][4*m]u64 =
  -- this looks a bit wierd compared to a size-coercien, but I could not get it
  -- to fuse properly with a coercien, and it ran much slower
  let uss = map flatten usss :> [n][4*m]u64
  let vss = map flatten vsss :> [n][4*m]u64
  in oneMulV2 m uss vss

entry oneMulV3Bench64
[n][ipb][m] (usss: [n][ipb][m]u64) (vsss: [n][ipb][m]u64) : [n][ipb][m]u64 =
  oneMulV3 (m/4) (usss :> [n][ipb][4*(m/4)]u64) (vsss :> [n][ipb][4*(m/4)]u64)
           :> [n][ipb][m]u64

entry sixMulV1Bench64
[n][m] (usss: [n][2][m]u64) (vsss: [n][2][m]u64) : [n][2*m]u64 =
  -- this looks a bit wierd compared to a size-coercien, but I could not get it
  -- to fuse properly with a coercien, and it ran much slower
  let uss = map flatten usss :> [n][2*m]u64
  let vss = map flatten vsss :> [n][2*m]u64
  in sixMulV1 m uss vss

entry sixMulV2Bench64
[n][m] (usss: [n][4][m]u64) (vsss: [n][4][m]u64) : [n][4*m]u64 =
  -- this looks a bit wierd compared to a size-coercien, but I could not get it
  -- to fuse properly with a coercien, and it ran much slower
  let uss = map flatten usss :> [n][4*m]u64
  let vss = map flatten vsss :> [n][4*m]u64
  in sixMulV2 m uss vss

entry sixMulV3Bench64
[n][ipb][m] (usss: [n][ipb][m]u64) (vsss: [n][ipb][m]u64) : [n][ipb][m]u64 =
  sixMulV3 (m/4) (usss :> [n][ipb][4*(m/4)]u64) (vsss :> [n][ipb][4*(m/4)]u64)
           :> [n][ipb][m]u64
