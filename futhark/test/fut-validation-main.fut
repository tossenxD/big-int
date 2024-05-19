import "gmp-validation-lib"
import "../helper"
import "../add"
import "../mul"

-- This file contains "polymorphic" validation tests. Well, they are actually
-- not polymorphic, but rather use the chosen base type. However, a test file
-- `fut-validation-FOO.fut` can safely run its tests without any manual effort,
-- except changing the `ui` type to FOO in `helper.fut`, which would have to be
-- done anyway for trying different bases. Pseudo-polymorphic?.. It relies on
-- `gmp-validation-lib.fut` to export arithmetics validated by GMP.

def add1D [m] (f: ui -> ui -> bool) (u: [m]ui) (v: [m]ui) : bool =
  let M = m/4
  let validP = eq f (test_add u v)
  let w0 = add u v
  let w1 = baddV1 u v
  let w2 = baddV2 (u :> [4*M]ui) (v :> [4*M]ui) :> [m]ui
  let w3 = baddV3 (u :> [1*(4*M)]ui) (v :> [1*(4*M)]ui) :> [m]ui
  in validP w0 && validP w1 && validP w2 && validP w3

def add2D [n][m] (f: ui -> ui -> bool) (us: [n][m]ui) (vs: [n][m]ui) : bool =
  let (N, M) = (n/4, m/4)
  let validP = (\ws -> map2 (eq f) (map2 test_add us vs) ws |> reduce (&&) true)
  let ws0 = oneAddV0 us vs
  let ws1 = oneAddV1 us vs
  let ws2 = oneAddV2 M (us :> [n][4*M]ui) (vs :> [n][4*M]ui) :>[n][m]ui
  let usV3 = unflatten (us :> [N*4][4*M]ui) :> [N][4][4*M]ui
  let vsV3 = unflatten (vs :> [N*4][4*M]ui) :> [N][4][4*M]ui
  let ws3 = (oneAddV3 M usV3 vsV3 |> flatten) :> [n][m]ui
  in validP ws0 && validP ws1 && validP ws2 && validP ws3

def mul1D [m] (f: ui -> ui -> bool) (u: [m]ui) (v: [m]ui) : bool =
  let (M2, M4) = (m/2, m/4)
  let validP = eq f (test_mul u v)
  let w1 = convMultV1 (u :> [2*M2]ui) (v :> [2*M2]ui) :> [m]ui
  let w2 = convMultV2 (u :> [4*M4]ui) (v :> [4*M4]ui) :> [m]ui
  let w3 = convMultV3 (u :> [1*(4*M4)]ui) (v :> [1*(4*M4)]ui) :> [m]ui
  in validP w1 && validP w2 && validP w3

def mul2D [n][m] (f: ui -> ui -> bool) (us: [n][m]ui) (vs: [n][m]ui) : bool =
  let (N, M2, M4) = (n/4, m/2, m/4)
  let validP = (\ws -> map2 (eq f) (map2 test_mul us vs) ws |> reduce (&&) true)
  let ws1 = oneMulV1 M2 (us :> [n][2*M2]ui) (vs :> [n][2*M2]ui) :>[n][m]ui
  let ws2 = oneMulV2 M4 (us :> [n][4*M4]ui) (vs :> [n][4*M4]ui) :>[n][m]ui
  let usV3 = unflatten (us :> [N*4][4*M4]ui) :> [N][4][4*M4]ui
  let vsV3 = unflatten (vs :> [N*4][4*M4]ui) :> [N][4][4*M4]ui
  let ws3 = (oneMulV3 M4 usV3 vsV3 |> flatten) :> [n][m]ui
  in validP ws1 && validP ws2 && validP ws3