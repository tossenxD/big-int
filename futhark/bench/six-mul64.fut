import "../mul"
import "../helper"

-- ==
-- entry: six_mul64v1
-- compiled random input { [65536][1024]u64 [65536][1024]u64 }
-- compiled random input { [131072][512]u64 [131072][512]u64 }
-- compiled random input { [262144][256]u64 [262144][256]u64 }
-- compiled random input { [524288][128]u64 [524288][128]u64 }
-- compiled random input { [1048576][64]u64 [1048576][64]u64 }
-- compiled random input { [2097152][32]u64 [2097152][32]u64 }
-- compiled random input { [4194304][16]u64 [4194304][16]u64 }
-- compiled random input { [8388608][8]u64  [8388608][8]u64  }
entry six_mul64v1 [m][n] (ass: [m][n]u64) (bss: [m][n]u64) : [m][n]u64 =
  let assbss = imap2Intra ass bss convMult64v1
  in loop rss = assbss for i < 5 do imap2Intra ass rss convMult64v1

-- ==
-- entry: six_mul64v2
-- compiled random input { [65536][1024]u64 [65536][1024]u64 }
-- compiled random input { [131072][512]u64 [131072][512]u64 }
-- compiled random input { [262144][256]u64 [262144][256]u64 }
-- compiled random input { [524288][128]u64 [524288][128]u64 }
-- compiled random input { [1048576][64]u64 [1048576][64]u64 }
-- compiled random input { [2097152][32]u64 [2097152][32]u64 }
-- compiled random input { [4194304][16]u64 [4194304][16]u64 }
-- compiled random input { [8388608][8]u64  [8388608][8]u64  }
entry six_mul64v2 [m][n] (ass: [m][n]u64) (bss: [m][n]u64) : [m][n]u64 =
  let assbss = imap2Intra ass bss convMult64v2
  in loop rss = assbss for i < 5 do imap2Intra ass rss convMult64v2
