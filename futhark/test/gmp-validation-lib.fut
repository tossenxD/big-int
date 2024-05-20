import "../helper"
import "../add"
import "../sub"
import "../mul"
import "../div"

-- This file exports arithmetics to be validated against GMP by the program
-- `gmp-validation.c`. Hence, other Futhark tests (see `fut-validation-lib.fut`)
-- can safely assume that functions exported here are correct. Functions are
-- agnostic to the size of the big integers (array shapes). They are supposed to
-- be treated as black box.

entry test_add [m] (u: [m]ui) (v: [m]ui) : [m]ui =
  baddV1 u v

entry test_sub [m] (u: [m]ui) (v: [m]ui) : [m]ui =
  bsub u v |> fst

entry test_mul [m] (u: [m]ui) (v: [m]ui) : [m]ui =
  let p = (4 - (m % 4)) % 4 -- add padding to ensure alignment
  let pz = replicate p 0
  let up = u ++ pz :> [4*((m+p)/4)]ui
  let vp = v ++ pz :> [4*((m+p)/4)]ui
  in (convMultV2 up vp :> [m+p]ui) |> take m

entry test_div [m] (u: [m]ui) (v: [m]ui) : [m]ui =
  div u v |> fst
