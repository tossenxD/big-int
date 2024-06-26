import "gmp-validation-lib"
import "../helper"
import "../add"
import "../mul"

-- Like `fut-validation-main.fut', but testing of properties. (See the
-- validation file for details).

def addIdentity [m] (u: [m]ui) : bool =
  let M = m/4
  let id = replicate m 0
  let b0 = eq (add u id) u
  let b1 = eq (baddV1 u id) u
  let b2 = eq ((baddV2 (u :> [4*M]ui) (id :> [4*M]ui)) :> [m]ui) u
  let b3 = eq ((baddV3 (u :> [1*(4*M)]ui) (id :> [1*(4*M)]ui)) :> [m]ui) u
  let b4 = eq ((baddV4 (u :> [1*m]ui) (id :> [1*m]ui)) :> [m]ui) u
  in b0 && b1 && b2 && b3 && b4

def addCommutative [m] (u: [m]ui) (v: [m]ui) : bool =
  let M = m/4
  let b0 = eq (add u v)
              (add v u)
  let b1 = eq (baddV1 u v)
              (baddV1 v u)
  let b2 = eq (baddV2 (u :> [4*M]ui) (v :> [4*M]ui))
              (baddV2 (v :> [4*M]ui) (u :> [4*M]ui))
  let b3 = eq (baddV3 (u :> [1*(4*M)]ui) (v :> [1*(4*M)]ui))
              (baddV3 (v :> [1*(4*M)]ui) (u :> [1*(4*M)]ui))
  let b4 = eq (baddV4 (u :> [1*m]ui) (v :> [1*m]ui))
              (baddV4 (v :> [1*m]ui) (u :> [1*m]ui))
  in b0 && b1 && b2 && b3 && b4

def addAssociative [m] (s: [m]ui) (u: [m]ui) (v: [m]ui) : bool =
  let M = m/4
  let b0 = eq (add (add s u) v)
              (add s (add u v))
  let b1 = eq (baddV1 (baddV1 s u) v)
              (baddV1 s (baddV1 u v))
  let b2 = eq (baddV2 (baddV2 (s :> [4*M]ui) (u :> [4*M]ui)) (v :> [4*M]ui))
              (baddV2 (s :> [4*M]ui) (baddV2 (u :> [4*M]ui) (v :> [4*M]ui)))
  let b3 = eq (baddV3 (baddV3 (s :> [1*(4*M)]ui) (u :> [1*(4*M)]ui)) (v :> [1*(4*M)]ui))
              (baddV3 (s :> [1*(4*M)]ui) (baddV3 (u :> [1*(4*M)]ui) (v :> [1*(4*M)]ui)))
  let b4 = eq (baddV4 (baddV4 (s :> [1*m]ui) (u :> [1*m]ui)) (v :> [1*m]ui))
              (baddV4 (s :> [1*m]ui) (baddV4 (u :> [1*m]ui) (v :> [1*m]ui)))
  in b0 && b1 && b2 && b3 && b4

def mulIdentity [m] (u: [m]ui) : bool =
  let (M2, M4) = (m/2, m/4)
  let id = map (\ i -> if i == 0 then 1 else 0) (iota m)
  let b1 = eq ((convMulV1 (u :> [2*M2]ui) (id :> [2*M2]ui)) :> [m]ui) u
  let b2 = eq ((convMulV2 (u :> [4*M4]ui) (id :> [4*M4]ui)) :> [m]ui) u
  let b3 = eq ((convMulV3 (u :> [1*(4*M4)]ui) (id :> [1*(4*M4)]ui)) :> [m]ui) u
  let b4 = eq ((convMulV4 (u :> [1*(2*M2)]ui) (id :> [1*(2*M2)]ui)) :> [m]ui) u
  in b1 && b2 && b3 && b4

def mulZero [m] (u: [m]ui) : bool =
  let (M2, M4) = (m/2, m/4)
  let z  = replicate m 0
  let b1 = eq ((convMulV1 (u :> [2*M2]ui) (z :> [2*M2]ui)) :> [m]ui) z
  let b2 = eq ((convMulV2 (u :> [4*M4]ui) (z :> [4*M4]ui)) :> [m]ui) z
  let b3 = eq ((convMulV3 (u :> [1*(4*M4)]ui) (z :> [1*(4*M4)]ui)) :> [m]ui) z
  let b4 = eq ((convMulV4 (u :> [1*(2*M2)]ui) (z :> [1*(2*M2)]ui)) :> [m]ui) z
  in b1 && b2 && b3 && b4

def mulCommutative [m] (u: [m]ui) (v: [m]ui) : bool =
  let (M2, M4) = (m/2, m/4)
  let b1 = eq (convMulV1 (u :> [2*M2]ui) (v :> [2*M2]ui))
              (convMulV1 (v :> [2*M2]ui) (u :> [2*M2]ui))
  let b2 = eq (convMulV2 (u :> [4*M4]ui) (v :> [4*M4]ui))
              (convMulV2 (v :> [4*M4]ui) (u :> [4*M4]ui))
  let b3 = eq (convMulV3 (u :> [1*(4*M4)]ui) (v :> [1*(4*M4)]ui))
              (convMulV3 (v :> [1*(4*M4)]ui) (u :> [1*(4*M4)]ui))
  let b4 = eq (convMulV4 (u :> [1*(2*M2)]ui) (v :> [1*(2*M2)]ui))
              (convMulV4 (v :> [1*(2*M2)]ui) (u :> [1*(2*M2)]ui))
  in b1 && b2 && b3 && b4

def mulAssociative [m] (s: [m]ui) (u: [m]ui) (v: [m]ui) : bool =
  let (M2, M4) = (m/2, m/4)
  let b1 = eq (convMulV1 (convMulV1 (s :> [2*M2]ui) (u :> [2*M2]ui)) (v :> [2*M2]ui))
              (convMulV1 (s :> [2*M2]ui) (convMulV1 (u :> [2*M2]ui) (v :> [2*M2]ui)))
  let b2 = eq (convMulV2 (convMulV2 (s :> [4*M4]ui) (u :> [4*M4]ui)) (v :> [4*M4]ui))
              (convMulV2 (s :> [4*M4]ui) (convMulV2 (u :> [4*M4]ui) (v :> [4*M4]ui)))
  let b3 = eq (convMulV3 (convMulV3 (s :> [1*(4*M4)]ui) (u :> [1*(4*M4)]ui)) (v :> [1*(4*M4)]ui))
              (convMulV3 (s :> [1*(4*M4)]ui) (convMulV3 (u :> [1*(4*M4)]ui) (v :> [1*(4*M4)]ui)))
  let b4 = eq (convMulV4 (convMulV4 (s :> [1*(2*M2)]ui) (u :> [1*(2*M2)]ui)) (v :> [1*(2*M2)]ui))
              (convMulV4 (s :> [1*(2*M2)]ui) (convMulV4 (u :> [1*(2*M2)]ui) (v :> [1*(2*M2)]ui)))
  in b1 && b2 && b3 && b4

def mulDistributive [m] (s: [m]ui) (u: [m]ui) (v: [m]ui) : bool =
  let (M2, M4) = (m/2, m/4)
  let b1 = eq (convMulV1 (s :> [2*M2]ui) ((test_add u v) :> [2*M2]ui))
              (test_add (convMulV1 (s :> [2*M2]ui) (u :> [2*M2]ui)) (convMulV1 (s :> [2*M2]ui) (v :> [2*M2]ui)))
  let b2 = eq (convMulV2 (s :> [4*M4]ui) ((test_add u v) :> [4*M4]ui))
              (test_add (convMulV2 (s :> [4*M4]ui) (u :> [4*M4]ui)) (convMulV2 (s :> [4*M4]ui) (v :> [4*M4]ui)))
  let b3 = eq (convMulV3 (s :> [1*(4*M4)]ui) ((test_add u v) :> [1*(4*M4)]ui))
              (test_add (convMulV3 (s :> [1*(4*M4)]ui) (u :> [1*(4*M4)]ui)) (convMulV3 (s :> [1*(4*M4)]ui) (v :> [1*(4*M4)]ui)))
  let b4 = eq (convMulV4 (s :> [1*(2*M2)]ui) ((test_add u v) :> [1*(2*M2)]ui))
              (test_add (convMulV4 (s :> [1*(2*M2)]ui) (u :> [1*(2*M2)]ui)) (convMulV4 (s :> [1*(2*M2)]ui) (v :> [1*(2*M2)]ui)))
  in b1 && b2 && b3 && b4
