import "helper"
import "add"

--------------------------------------------------------------------------------
--- Big Integer Subtraction
--------------------------------------------------------------------------------
--- Defined from addition, see `add.fut` for more information.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Based on baddV1; false is `+`, true is `-`
--------------------------------------------------------------------------------

def bsub [m] (us: [m]ui) (vs: [m]ui) : ([m]ui, bool) =
  -- 1. compute sign
  let (us, vs, sign) = if lt us vs then (vs, us, true) else (us, vs, false)

  -- 2. compute absolute sums and carries
  let (ws, cs) = unzip <| imap2 us vs
    (\ u v -> let w = u-v in (w, (boolToCt (w > u)) | ((boolToCt (w==0)) << 1)))

  -- 3. propagate carries
  let pcs = scanExc carryProp carryPropE cs

  -- 4. add carries to sums
  in (map2 (\ w c -> w - fromCt (c & 1)) ws pcs, sign)
