import "../add"

-- Benchmarks with one instance per block and sequentialization factor of 1
-- ==
-- entry: oneAddV0Bench32 tenAddV0Bench32 oneAddV1Bench32 tenAddV1Bench32
-- compiled random input { [16384][8192]u32  [16384][8192]u32  }
-- compiled random input { [32768][4096]u32  [32768][4096]u32  }
-- compiled random input { [65536][2048]u32  [65536][2048]u32  }
-- compiled random input { [131072][1024]u32 [131072][1024]u32 }
-- compiled random input { [262144][512]u32  [262144][512]u32  }
-- compiled random input { [524288][256]u32  [524288][256]u32  }
-- compiled random input { [1048576][128]u32 [1048576][128]u32 }
-- compiled random input { [2097152][64]u32  [2097152][64]u32  }
-- compiled random input { [4194304][32]u32  [4194304][32]u32  }
-- compiled random input { [8388608][16]u32  [8388608][16]u32  }

-- Benchmarks with one instance per block and sequentialization factor of 4
-- ==
-- entry: oneAddV2Bench32 tenAddV2Bench32
-- compiled random input { [16384][4][2048]u32 [16384][4][2048]u32 }
-- compiled random input { [32768][4][1024]u32 [32768][4][1024]u32 }
-- compiled random input { [65536][4][512]u32  [65536][4][512]u32  }
-- compiled random input { [131072][4][256]u32 [131072][4][256]u32 }
-- compiled random input { [262144][4][128]u32 [262144][4][128]u32 }
-- compiled random input { [524288][4][64]u32  [524288][4][64]u32  }
-- compiled random input { [1048576][4][32]u32 [1048576][4][32]u32 }
-- compiled random input { [2097152][4][16]u32 [2097152][4][16]u32 }
-- compiled random input { [4194304][4][8]u32  [4194304][4][8]u32  }
-- compiled random input { [8388608][4][4]u32  [8388608][4][4]u32  }

-- Benchmarks with multiple instances per block and sequentialization factor 4
-- ==
-- entry: oneAddV3Bench32 tenAddV3Bench32
-- compiled random input { [16384][1][8192]u32  [16384][1][8192]u32  }
-- compiled random input { [32768][1][4096]u32  [32768][1][4096]u32  }
-- compiled random input { [65536][1][2048]u32  [65536][1][2048]u32  }
-- compiled random input { [131072][1][1024]u32 [131072][1][1024]u32 }
-- compiled random input { [262144][1][512]u32  [262144][1][512]u32  }
-- compiled random input { [262144][2][256]u32  [262144][2][256]u32  }
-- compiled random input { [262144][4][128]u32  [262144][4][128]u32  }
-- compiled random input { [262144][8][64]u32   [262144][8][64]u32   }
-- compiled random input { [262144][16][32]u32  [262144][16][32]u32  }
-- compiled random input { [262144][32][16]u32  [262144][32][16]u32  }

entry oneAddV0Bench32 [n][m] (uss: [n][m]u32) (vss: [n][m]u32) : [n][m]u32 =
  oneAddV0 uss vss

entry oneAddV1Bench32 [n][m] (uss: [n][m]u32) (vss: [n][m]u32) : [n][m]u32 =
  oneAddV1 uss vss

entry oneAddV2Bench32
[n][m] (usss: [n][4][m]u32) (vsss: [n][4][m]u32) : [n][4*m]u32 =
  -- this looks a bit wierd compared to a size-coercien, but I could not get it
  -- to fuse properly with a coercien, and it ran much slower
  let uss = map flatten usss :> [n][4*m]u32
  let vss = map flatten vsss :> [n][4*m]u32
  in oneAddV2 m uss vss

entry oneAddV3Bench32
[n][ipb][m] (usss: [n][ipb][m]u32) (vsss: [n][ipb][m]u32) : [n][ipb][m]u32 =
  oneAddV3 (m/4) (usss :> [n][ipb][4*(m/4)]u32) (vsss :> [n][ipb][4*(m/4)]u32)
           :> [n][ipb][m]u32

entry tenAddV0Bench32 [n][m] (uss: [n][m]u32) (vss: [n][m]u32) : [n][m]u32 =
  tenAddV0 uss vss

entry tenAddV1Bench32 [n][m] (uss: [n][m]u32) (vss: [n][m]u32) : [n][m]u32 =
  tenAddV1 uss vss

entry tenAddV2Bench32
[n][m] (usss: [n][4][m]u32) (vsss: [n][4][m]u32) : [n][4*m]u32 =
  -- this looks a bit wierd compared to a size-coercien, but I could not get it
  -- to fuse properly with a coercien, and it ran much slower
  let uss = map flatten usss :> [n][4*m]u32
  let vss = map flatten vsss :> [n][4*m]u32
  in tenAddV2 m uss vss

entry tenAddV3Bench32
[n][ipb][m] (usss: [n][ipb][m]u32) (vsss: [n][ipb][m]u32) : [n][ipb][m]u32 =
  tenAddV3 (m/4) (usss :> [n][ipb][4*(m/4)]u32) (vsss :> [n][ipb][4*(m/4)]u32)
           :> [n][ipb][m]u32
