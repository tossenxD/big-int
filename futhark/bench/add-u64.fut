import "../add"

-- Benchmarks with one instance per block and sequentialization factor of 1
-- ==
-- entry: oneAddV0Bench64 tenAddV0Bench64 oneAddV1Bench64 tenAddV1Bench64
-- compiled random input { [16384][4096]u64 [16384][4096]u64 }
-- compiled random input { [32768][2048]u64 [32768][2048]u64 }
-- compiled random input { [65536][1024]u64 [65536][1024]u64 }
-- compiled random input { [131072][512]u64 [131072][512]u64 }
-- compiled random input { [262144][256]u64 [262144][256]u64 }
-- compiled random input { [524288][128]u64 [524288][128]u64 }
-- compiled random input { [1048576][64]u64 [1048576][64]u64 }
-- compiled random input { [2097152][32]u64 [2097152][32]u64 }
-- compiled random input { [4194304][16]u64 [4194304][16]u64 }
-- compiled random input { [8388608][8]u64  [8388608][8]u64  }

-- Benchmarks with one instance per block and sequentialization factor of 4
-- ==
-- entry: oneAddV2Bench64 tenAddV2Bench64
-- compiled random input { [16384][4][1024]u64 [16384][4][1024]u64 }
-- compiled random input { [32768][4][512]u64  [32768][4][512]u64  }
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
-- entry: oneAddV3Bench64 tenAddV3Bench64 oneAddV4Bench64 tenAddV4Bench64
-- compiled random input { [16384][1][4096]u64 [16384][1][4096]u64 }
-- compiled random input { [32768][1][2048]u64 [32768][1][2048]u64 }
-- compiled random input { [65536][1][1024]u64 [65536][1][1024]u64 }
-- compiled random input { [131072][1][512]u64 [131072][1][512]u64 }
-- compiled random input { [262144][1][256]u64 [262144][1][256]u64 }
-- compiled random input { [262144][2][128]u64 [262144][2][128]u64 }
-- compiled random input { [262144][4][64]u64  [262144][4][64]u64  }
-- compiled random input { [262144][8][32]u64  [262144][8][32]u64  }
-- compiled random input { [262144][16][16]u64 [262144][16][16]u64 }
-- compiled random input { [262144][32][8]u64  [262144][32][8]u64  }

entry oneAddV0Bench64 [n][m] (uss: [n][m]u64) (vss: [n][m]u64) : [n][m]u64 =
  oneAddV0 uss vss

entry oneAddV1Bench64 [n][m] (uss: [n][m]u64) (vss: [n][m]u64) : [n][m]u64 =
  oneAddV1 uss vss

entry oneAddV2Bench64
[n][m] (usss: [n][4][m]u64) (vsss: [n][4][m]u64) : [n][4*m]u64 =
  -- this looks a bit wierd compared to a size-coercien, but I could not get it
  -- to fuse properly with a coercien, and it ran much slower
  let uss = map flatten usss :> [n][4*m]u64
  let vss = map flatten vsss :> [n][4*m]u64
  in oneAddV2 m uss vss

entry oneAddV3Bench64
[n][ipb][m] (usss: [n][ipb][m]u64) (vsss: [n][ipb][m]u64) : [n][ipb][m]u64 =
  oneAddV3 (m/4) (usss :> [n][ipb][4*(m/4)]u64) (vsss :> [n][ipb][4*(m/4)]u64)
           :> [n][ipb][m]u64

entry oneAddV4Bench64
[n][ipb][m] (usss: [n][ipb][m]u64) (vsss: [n][ipb][m]u64) : [n][ipb][m]u64 =
  oneAddV4 m usss vsss

entry tenAddV0Bench64 [n][m] (uss: [n][m]u64) (vss: [n][m]u64) : [n][m]u64 =
  tenAddV0 uss vss

entry tenAddV1Bench64 [n][m] (uss: [n][m]u64) (vss: [n][m]u64) : [n][m]u64 = 
  tenAddV1 uss vss

entry tenAddV2Bench64
[n][m] (usss: [n][4][m]u64) (vsss: [n][4][m]u64) : [n][4*m]u64 =
  -- this looks a bit wierd compared to a size-coercien, but I could not get it
  -- to fuse properly with a coercien, and it ran much slower
  let uss = map flatten usss :> [n][4*m]u64
  let vss = map flatten vsss :> [n][4*m]u64
  in tenAddV2 m uss vss

entry tenAddV3Bench64
[n][ipb][m] (usss: [n][ipb][m]u64) (vsss: [n][ipb][m]u64) : [n][ipb][m]u64 =
  tenAddV3 (m/4) (usss :> [n][ipb][4*(m/4)]u64) (vsss :> [n][ipb][4*(m/4)]u64)
           :> [n][ipb][m]u64

entry tenAddV4Bench64
[n][ipb][m] (usss: [n][ipb][m]u64) (vsss: [n][ipb][m]u64) : [n][ipb][m]u64 =
  tenAddV4 m usss vsss
