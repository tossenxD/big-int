import "gmp-validation-lib"
import "../helper"
import "../add"
import "../mul"

-- Like `fut-validation-main.fut', but property testing. (See file for details).

def addIdentity [m] (u: [m]ui) : bool =
  let M = m/4
  let id = replicate m 0
  let b0 = eq (add u id) u
  let b1 = eq (baddV1 u id) u
  let b2 = eq ((baddV2 (u :> [4*M]ui) (id :> [4*M]ui)) :> [m]ui) u
  let b3 = eq ((baddV3 (u :> [1*(4*M)]ui) (id :> [1*(4*M)]ui)) :> [m]ui) u
  in b0 && b1 && b2 && b3

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
  in b0 && b1 && b2 && b3

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
  in b0 && b1 && b2 && b3

def mulIdentity [m] (u: [m]ui) : bool =
  let (M2, M4) = (m/2, m/4)
  let id = map (\ i -> if i == 0 then 1 else 0) (iota m)
  let b1 = eq ((convMultV1 (u :> [2*M2]ui) (id :> [2*M2]ui)) :> [m]ui) u
  let b2 = eq ((convMultV2 (u :> [4*M4]ui) (id :> [4*M4]ui)) :> [m]ui) u
  let b3 = eq ((convMultV3 (u :> [1*(4*M4)]ui) (id :> [1*(4*M4)]ui)) :> [m]ui) u
  in b1 && b2 && b3

def mulZero [m] (u: [m]ui) : bool =
  let (M2, M4) = (m/2, m/4)
  let z  = replicate m 0
  let b1 = eq ((convMultV1 (u :> [2*M2]ui) (z :> [2*M2]ui)) :> [m]ui) z
  let b2 = eq ((convMultV2 (u :> [4*M4]ui) (z :> [4*M4]ui)) :> [m]ui) z
  let b3 = eq ((convMultV3 (u :> [1*(4*M4)]ui) (z :> [1*(4*M4)]ui)) :> [m]ui) z
  in b1 && b2 && b3

def mulCommutative [m] (u: [m]ui) (v: [m]ui) : bool =
  let (M2, M4) = (m/2, m/4)
  let b1 = eq (convMultV1 (u :> [2*M2]ui) (v :> [2*M2]ui))
              (convMultV1 (v :> [2*M2]ui) (u :> [2*M2]ui))
  let b2 = eq (convMultV2 (u :> [4*M4]ui) (v :> [4*M4]ui))
              (convMultV2 (v :> [4*M4]ui) (u :> [4*M4]ui))
  let b3 = eq (convMultV3 (u :> [1*(4*M4)]ui) (v :> [1*(4*M4)]ui))
              (convMultV3 (v :> [1*(4*M4)]ui) (u :> [1*(4*M4)]ui))
  in b1 && b2 && b3

def mulAssociative [m] (s: [m]ui) (u: [m]ui) (v: [m]ui) : bool =
  let (M2, M4) = (m/2, m/4)
  let b1 = eq (convMultV1 (convMultV1 (s :> [2*M2]ui) (u :> [2*M2]ui)) (v :> [2*M2]ui))
              (convMultV1 (s :> [2*M2]ui) (convMultV1 (u :> [2*M2]ui) (v :> [2*M2]ui)))
  let b2 = eq (convMultV2 (convMultV2 (s :> [4*M4]ui) (u :> [4*M4]ui)) (v :> [4*M4]ui))
              (convMultV2 (s :> [4*M4]ui) (convMultV2 (u :> [4*M4]ui) (v :> [4*M4]ui)))
  let b3 = eq (convMultV3 (convMultV3 (s :> [1*(4*M4)]ui) (u :> [1*(4*M4)]ui)) (v :> [1*(4*M4)]ui))
              (convMultV3 (s :> [1*(4*M4)]ui) (convMultV3 (u :> [1*(4*M4)]ui) (v :> [1*(4*M4)]ui)))
  in b1 && b2 && b3

def mulDistributive [m] (s: [m]ui) (u: [m]ui) (v: [m]ui) : bool =
  let (M2, M4) = (m/2, m/4)
  let b1 = eq (convMultV1 (s :> [2*M2]ui) ((test_add u v) :> [2*M2]ui))
              (test_add (convMultV1 (s :> [2*M2]ui) (u :> [2*M2]ui)) (convMultV1 (s :> [2*M2]ui) (v :> [2*M2]ui)))
  let b2 = eq (convMultV2 (s :> [4*M4]ui) ((test_add u v) :> [4*M4]ui))
              (test_add (convMultV2 (s :> [4*M4]ui) (u :> [4*M4]ui)) (convMultV2 (s :> [4*M4]ui) (v :> [4*M4]ui)))
  let b3 = eq (convMultV3 (s :> [1*(4*M4)]ui) ((test_add u v) :> [1*(4*M4)]ui))
              (test_add (convMultV3 (s :> [1*(4*M4)]ui) (u :> [1*(4*M4)]ui)) (convMultV3 (s :> [1*(4*M4)]ui) (v :> [1*(4*M4)]ui)))
  in b1 && b2 && b3
