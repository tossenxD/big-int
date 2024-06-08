import "../mul"

-- Benchmarks with one instance per block and sequentialization factor of 2
-- ==
-- entry: oneMulV1Bench32 sixMulV1Bench32
-- compiled random input { [16384][2][4096]u32  [16384][2][4096]u32 }
-- compiled random input { [32768][2][2048]u32  [32768][2][2048]u32 }
-- compiled random input { [65536][2][1024]u32  [65536][2][1024]u32 }
-- compiled random input { [131072][2][512]u32  [131072][2][512]u32 }
-- compiled random input { [262144][2][256]u32  [262144][2][256]u32 }
-- compiled random input { [524288][2][128]u32  [524288][2][128]u32 }
-- compiled random input { [1048576][2][64]u32  [1048576][2][64]u32 }
-- compiled random input { [2097152][2][32]u32  [2097152][2][32]u32 }
-- compiled random input { [4194304][2][16]u32  [4194304][2][16]u32 }
-- compiled random input { [8388608][2][8]u32   [8388608][2][8]u32  }

-- Benchmarks with one instance per block and sequentialization factor of 4
-- ==
-- entry: oneMulV2Bench32 sixMulV2Bench32
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

-- Benchmarks with multiple instances per block
-- ==
-- entry: oneMulV3Bench32 sixMulV3Bench32 oneMulV4Bench32 sixMulV4Bench32
-- compiled random input { [16384][1][8196]u32  [16384][1][8196]u32  }
-- compiled random input { [32768][1][4096]u32  [32768][1][4096]u32  }
-- compiled random input { [65536][1][2048]u32  [65536][1][2048]u32  }
-- compiled random input { [131072][1][1024]u32 [131072][1][1024]u32 }
-- compiled random input { [262144][1][512]u32  [262144][1][512]u32  }
-- compiled random input { [262144][2][256]u32  [262144][2][256]u32  }
-- compiled random input { [262144][4][128]u32  [262144][4][128]u32  }
-- compiled random input { [262144][8][64]u32   [262144][8][64]u32   }
-- compiled random input { [262144][16][32]u32  [262144][16][32]u32  }
-- compiled random input { [262144][32][16]u32  [262144][32][16]u32  }

entry oneMulV1Bench32
[n][m] (usss: [n][2][m]u32) (vsss: [n][2][m]u32) : [n][2*m]u32 =
  -- this looks a bit wierd compared to a size-coercien, but I could not get it
  -- to fuse properly with a coercien, and it ran much slower
  let uss = map flatten usss :> [n][2*m]u32
  let vss = map flatten vsss :> [n][2*m]u32
  in oneMulV1 m uss vss

entry oneMulV2Bench32
[n][m] (usss: [n][4][m]u32) (vsss: [n][4][m]u32) : [n][4*m]u32 =
  -- this looks a bit wierd compared to a size-coercien, but I could not get it
  -- to fuse properly with a coercien, and it ran much slower
  let uss = map flatten usss :> [n][4*m]u32
  let vss = map flatten vsss :> [n][4*m]u32
  in oneMulV2 m uss vss

entry oneMulV3Bench32
[n][ipb][m] (usss: [n][ipb][m]u32) (vsss: [n][ipb][m]u32) : [n][ipb][m]u32 =
  oneMulV3 (m/4) (usss :> [n][ipb][4*(m/4)]u32) (vsss :> [n][ipb][4*(m/4)]u32)
           :> [n][ipb][m]u32

entry oneMulV4Bench32
[n][ipb][m] (usss: [n][ipb][m]u32) (vsss: [n][ipb][m]u32) : [n][ipb][m]u32 =
  oneMulV4 (m/2) (usss :> [n][ipb][2*(m/2)]u32) (vsss :> [n][ipb][2*(m/2)]u32)
           :> [n][ipb][m]u32

entry sixMulV1Bench32
[n][m] (usss: [n][2][m]u32) (vsss: [n][2][m]u32) : [n][2*m]u32 =
  -- this looks a bit wierd compared to a size-coercien, but I could not get it
  -- to fuse properly with a coercien, and it ran much slower
  let uss = map flatten usss :> [n][2*m]u32
  let vss = map flatten vsss :> [n][2*m]u32
  in sixMulV1 m uss vss

entry sixMulV2Bench32
[n][m] (usss: [n][4][m]u32) (vsss: [n][4][m]u32) : [n][4*m]u32 =
  -- this looks a bit wierd compared to a size-coercien, but I could not get it
  -- to fuse properly with a coercien, and it ran much slower
  let uss = map flatten usss :> [n][4*m]u32
  let vss = map flatten vsss :> [n][4*m]u32
  in sixMulV2 m uss vss

entry sixMulV3Bench32
[n][ipb][m] (usss: [n][ipb][m]u32) (vsss: [n][ipb][m]u32) : [n][ipb][m]u32 =
  sixMulV3 (m/4) (usss :> [n][ipb][4*(m/4)]u32) (vsss :> [n][ipb][4*(m/4)]u32)
           :> [n][ipb][m]u32

entry sixMulV4Bench32
[n][ipb][m] (usss: [n][ipb][m]u32) (vsss: [n][ipb][m]u32) : [n][ipb][m]u32 =
  sixMulV4 (m/2) (usss :> [n][ipb][2*(m/2)]u32) (vsss :> [n][ipb][2*(m/2)]u32)
           :> [n][ipb][m]u32
