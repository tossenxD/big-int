import "add"

-- ==
-- entry: ten_add32
-- compiled random input { [65536][2048]u32  [65536][2048]u32  }
-- compiled random input { [131072][1024]u32 [131072][1024]u32 }
-- compiled random input { [262144][512]u32  [262144][512]u32  }
-- compiled random input { [524288][256]u32  [524288][256]u32  }
-- compiled random input { [1048576][128]u32 [1048576][128]u32 }
-- compiled random input { [2097152][64]u32  [2097152][64]u32  }
-- compiled random input { [4194304][32]u32  [4194304][32]u32  }
-- compiled random input { [8388608][16]u32  [8388608][16]u32  }
entry ten_add32 [m][n] (ass: [m][n]u32) (bss: [m][n]u32) : [m][n]u32 =
  loop rss = bss for i < 10 do map2 add32 ass rss

-- ==
-- entry: ten_add64
-- compiled random input { [65536][2048]u64  [65536][2048]u64  }
-- compiled random input { [131072][1024]u64 [131072][1024]u64 }
-- compiled random input { [262144][512]u64  [262144][512]u64  }
-- compiled random input { [524288][256]u64  [524288][256]u64  }
-- compiled random input { [1048576][128]u64 [1048576][128]u64 }
-- compiled random input { [2097152][64]u64  [2097152][64]u64  }
-- compiled random input { [4194304][32]u64  [4194304][32]u64  }
-- compiled random input { [8388608][16]u64  [8388608][16]u64  }
entry ten_add64 [m][n] (ass: [m][n]u64) (bss: [m][n]u64) : [m][n]u64 =
  loop rss = bss for i < 10 do map2 add64 ass rss

-- ==
-- entry: ten_badd32v1
-- compiled random input { [65536][2048]u32  [65536][2048]u32  }
-- compiled random input { [131072][1024]u32 [131072][1024]u32 }
-- compiled random input { [262144][512]u32  [262144][512]u32  }
-- compiled random input { [524288][256]u32  [524288][256]u32  }
-- compiled random input { [1048576][128]u32 [1048576][128]u32 }
-- compiled random input { [2097152][64]u32  [2097152][64]u32  }
-- compiled random input { [4194304][32]u32  [4194304][32]u32  }
-- compiled random input { [8388608][16]u32  [8388608][16]u32  }
entry ten_badd32v1 [m][n] (ass: [m][n]u32) (bss: [m][n]u32) : [m][n]u32 =
  loop rss = bss for i < 10 do map2 badd32v1 ass rss

-- ==
-- entry: ten_badd64v1
-- compiled random input { [65536][2048]u64  [65536][2048]u64  }
-- compiled random input { [131072][1024]u64 [131072][1024]u64 }
-- compiled random input { [262144][512]u64  [262144][512]u64  }
-- compiled random input { [524288][256]u64  [524288][256]u64  }
-- compiled random input { [1048576][128]u64 [1048576][128]u64 }
-- compiled random input { [2097152][64]u64  [2097152][64]u64  }
-- compiled random input { [4194304][32]u64  [4194304][32]u64  }
-- compiled random input { [8388608][16]u64  [8388608][16]u64  }
entry ten_badd64v1 [m][n] (ass: [m][n]u64) (bss: [m][n]u64) : [m][n]u64 =
  loop rss = bss for i < 10 do map2 badd64v1 ass rss

-- ==
-- entry: ten_badd32v2
-- compiled random input { [65536][2048]u32  [65536][2048]u32  }
-- compiled random input { [131072][1024]u32 [131072][1024]u32 }
-- compiled random input { [262144][512]u32  [262144][512]u32  }
-- compiled random input { [524288][256]u32  [524288][256]u32  }
-- compiled random input { [1048576][128]u32 [1048576][128]u32 }
-- compiled random input { [2097152][64]u32  [2097152][64]u32  }
-- compiled random input { [4194304][32]u32  [4194304][32]u32  }
-- compiled random input { [8388608][16]u32  [8388608][16]u32  }
entry ten_badd32v2 [m][n] (ass: [m][n]u32) (bss: [m][n]u32) : [m][n]u32 =
  loop rss = bss for i < 10 do map2 badd32v2 ass rss

-- ==
-- entry: ten_badd64v2
-- compiled random input { [65536][2048]u64  [65536][2048]u64  }
-- compiled random input { [131072][1024]u64 [131072][1024]u64 }
-- compiled random input { [262144][512]u64  [262144][512]u64  }
-- compiled random input { [524288][256]u64  [524288][256]u64  }
-- compiled random input { [1048576][128]u64 [1048576][128]u64 }
-- compiled random input { [2097152][64]u64  [2097152][64]u64  }
-- compiled random input { [4194304][32]u64  [4194304][32]u64  }
-- compiled random input { [8388608][16]u64  [8388608][16]u64  }
entry ten_badd64v2 [m][n] (ass: [m][n]u64) (bss: [m][n]u64) : [m][n]u64 =
  loop rss = bss for i < 10 do map2 badd64v2 ass rss

-- ==
-- entry: ten_badd32v3
-- compiled random input { [65536][2048]u32  [65536][2048]u32  }
-- compiled random input { [131072][1024]u32 [131072][1024]u32 }
-- compiled random input { [262144][512]u32  [262144][512]u32  }
-- compiled random input { [524288][256]u32  [524288][256]u32  }
-- compiled random input { [1048576][128]u32 [1048576][128]u32 }
-- compiled random input { [2097152][64]u32  [2097152][64]u32  }
-- compiled random input { [4194304][32]u32  [4194304][32]u32  }
-- compiled random input { [8388608][16]u32  [8388608][16]u32  }
entry ten_badd32v3 [m][n] (ass: [m][n]u32) (bss: [m][n]u32) : [m][n]u32 =
  loop rss = bss for i < 10 do badd32v3Wrapper ass rss

-- ==
-- entry: ten_badd64v3
-- compiled random input { [65536][2048]u64  [65536][2048]u64  }
-- compiled random input { [131072][1024]u64 [131072][1024]u64 }
-- compiled random input { [262144][512]u64  [262144][512]u64  }
-- compiled random input { [524288][256]u64  [524288][256]u64  }
-- compiled random input { [1048576][128]u64 [1048576][128]u64 }
-- compiled random input { [2097152][64]u64  [2097152][64]u64  }
-- compiled random input { [4194304][32]u64  [4194304][32]u64  }
-- compiled random input { [8388608][16]u64  [8388608][16]u64  }
entry ten_badd64v3 [m][n] (ass: [m][n]u64) (bss: [m][n]u64) : [m][n]u64 =
  loop rss = bss for i < 10 do badd64v3Wrapper ass rss
