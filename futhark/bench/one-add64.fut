import "../add"

-- ==
-- entry: one_add64
-- compiled random input { [65536][2048]u64  [65536][2048]u64  }
-- compiled random input { [131072][1024]u64 [131072][1024]u64 }
-- compiled random input { [262144][512]u64  [262144][512]u64  }
-- compiled random input { [524288][256]u64  [524288][256]u64  }
-- compiled random input { [1048576][128]u64 [1048576][128]u64 }
-- compiled random input { [2097152][64]u64  [2097152][64]u64  }
-- compiled random input { [4194304][32]u64  [4194304][32]u64  }
-- compiled random input { [8388608][16]u64  [8388608][16]u64  }
entry one_add64 [m][n] (ass: [m][n]u64) (bss: [m][n]u64) : [m][n]u64 =
  map2 add64 ass bss

-- ==
-- entry: one_badd64v1
-- compiled random input { [65536][2048]u64  [65536][2048]u64  }
-- compiled random input { [131072][1024]u64 [131072][1024]u64 }
-- compiled random input { [262144][512]u64  [262144][512]u64  }
-- compiled random input { [524288][256]u64  [524288][256]u64  }
-- compiled random input { [1048576][128]u64 [1048576][128]u64 }
-- compiled random input { [2097152][64]u64  [2097152][64]u64  }
-- compiled random input { [4194304][32]u64  [4194304][32]u64  }
-- compiled random input { [8388608][16]u64  [8388608][16]u64  }
entry one_badd64v1 [m][n] (ass: [m][n]u64) (bss: [m][n]u64) : [m][n]u64 =
  map2 badd64v1 ass bss

-- ==
-- entry: one_badd64v2
-- compiled random input { [65536][2048]u64  [65536][2048]u64  }
-- compiled random input { [131072][1024]u64 [131072][1024]u64 }
-- compiled random input { [262144][512]u64  [262144][512]u64  }
-- compiled random input { [524288][256]u64  [524288][256]u64  }
-- compiled random input { [1048576][128]u64 [1048576][128]u64 }
-- compiled random input { [2097152][64]u64  [2097152][64]u64  }
-- compiled random input { [4194304][32]u64  [4194304][32]u64  }
-- compiled random input { [8388608][16]u64  [8388608][16]u64  }
entry one_badd64v2 [m][n] (ass: [m][n]u64) (bss: [m][n]u64) : [m][n]u64 =
  map2 badd64v2 ass bss

-- ==
-- entry: one_badd64v3
-- compiled random input { [65536][2048]u64  [65536][2048]u64  }
-- compiled random input { [131072][1024]u64 [131072][1024]u64 }
-- compiled random input { [262144][512]u64  [262144][512]u64  }
-- compiled random input { [524288][256]u64  [524288][256]u64  }
-- compiled random input { [1048576][128]u64 [1048576][128]u64 }
-- compiled random input { [2097152][64]u64  [2097152][64]u64  }
-- compiled random input { [4194304][32]u64  [4194304][32]u64  }
-- compiled random input { [8388608][16]u64  [8388608][16]u64  }
entry one_badd64v3 [m][n] (ass: [m][n]u64) (bss: [m][n]u64) : [m][n]u64 =
  badd64v3Wrapper ass bss
