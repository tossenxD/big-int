import "../mul"
import "../helper"

-- ==
-- entry: six_mul32v1
-- compiled random input { [65536][2048]u32  [65536][2048]u32  }
-- compiled random input { [131072][1024]u32 [131072][1024]u32 }
-- compiled random input { [262144][512]u32  [262144][512]u32  }
-- compiled random input { [524288][256]u32  [524288][256]u32  }
-- compiled random input { [1048576][128]u32 [1048576][128]u32 }
-- compiled random input { [2097152][64]u32  [2097152][64]u32  }
-- compiled random input { [4194304][32]u32  [4194304][32]u32  }
-- compiled random input { [8388608][16]u32  [8388608][16]u32  }
entry six_mul32v1 [m][n] (ass: [m][n]u32) (bss: [m][n]u32) : [m][n]u32 =
  let assbss = imap2Intra ass bss convMult32v1
  in loop rss = assbss for i < 5 do imap2Intra ass rss convMult32v1
