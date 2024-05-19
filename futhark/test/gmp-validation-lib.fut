import "../helper"
import "../add"
import "../mul"

-- This file exports arithmetics to be validated against GMP by the program
-- `gmp-validation.c`. Hence, other Futhark tests (see `fut-validation-lib.fut`)
-- can safely assume that functions exported here are correct. Functions are
-- agnostic to the size of the big integers (array shapes). They are supposed to
-- be treated as black box.

entry test_add [m] (as: [m]ui) (bs: [m]ui) : [m]ui =
  baddV1 as bs

entry test_mul [m] (as: [m]ui) (bs: [m]ui) : [m]ui =
  let p = (4 - (m % 4)) % 4 -- add padding to ensure alignment
  let pz = replicate p 0
  let asp = as ++ pz :> [4*((m+p)/4)]ui
  let bsp = bs ++ pz :> [4*((m+p)/4)]ui
  in (convMultV2 asp bsp :> [m+p]ui) |> take m
