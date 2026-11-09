-- SPDX-License-Identifier: MPL-2.0
-- Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
--
-- ESNSpectralScaling.idr — the reservoir scaling step is well defined, over Nat.
--
-- Re-homed from verification/proofs/lean4/ESNSpectralScaling.lean
-- (PROOF-POLICY section 3). The statements are the Lean file's; the Lean
-- proofs rested on the kernel axiom propext, which Idris2 does not have.
--
-- lib/neural/echo_state_network.ex scales the reservoir by
--   scale = reservoir_norm / max(max_row_sum, 0.001),
-- the induced infinity-norm (largest absolute row sum), not the spectral
-- radius. This file shows, over a Nat model, that the floor-guarded
-- denominator is positive whenever the floor is, so the scaling never
-- divides by zero. The echo-state property itself is a theory theorem,
-- planned in Agda over constructive reals
-- (docs/proofs/HANDOVER-neural-convergence.adoc); it is not proved here.

module ESNSpectralScaling

import Control.Relation
import Data.Nat

%default total

------------------------------------------------------------------------
-- Section 1: the model (entries are Nat magnitudes)
------------------------------------------------------------------------

||| A matrix as a list of rows.
public export
Matrix : Type
Matrix = List (List Nat)

||| Each row's sum of entry magnitudes (Enum.map(row, &abs/1) |> Enum.sum()).
public export
rowSums : Matrix -> List Nat
rowSums m = map sum m

||| The largest element, or the floor for an empty list
||| (Enum.max(fn -> 1.0 end)).
public export
maxAbsOr : List Nat -> (floor : Nat) -> Nat
maxAbsOr xs floor = foldl (\acc, x => if x > acc then x else acc) floor xs

||| The induced infinity-norm (largest row sum), with a floor.
public export
matrixMaxAbs : Matrix -> (floor : Nat) -> Nat
matrixMaxAbs m floor = maxAbsOr (rowSums m) floor

||| The scaling ratio of the target to the floor-clamped norm.
public export
scaleRatio : (target, maxAbs, floor : Nat) -> Nat
scaleRatio target maxAbs floor = div target (maximum maxAbs floor)

------------------------------------------------------------------------
-- Section 2: the empty matrix
------------------------------------------------------------------------

||| Property 1: an empty matrix has no row sums.
export
rowSumsNil : rowSums [] = []
rowSumsNil = Refl

||| Property 2: maxAbsOr of an empty list is the floor.
export
maxAbsOrNil : (floor : Nat) -> maxAbsOr [] floor = floor
maxAbsOrNil _ = Refl

||| Property 3: an empty matrix's norm is its floor.
export
matrixMaxAbsNil : (floor : Nat) -> matrixMaxAbs [] floor = floor
matrixMaxAbsNil _ = Refl

------------------------------------------------------------------------
-- Section 3: non-negativity
------------------------------------------------------------------------

||| Property 4: the norm is non-negative.
export
matrixMaxAbsNonneg : (m : Matrix) -> (floor : Nat) -> LTE 0 (matrixMaxAbs m floor)
matrixMaxAbsNonneg _ _ = LTEZero

||| Property 5: the scaling ratio is non-negative.
export
scaleRatioNonneg : (target, maxAbs, floor : Nat) -> LTE 0 (scaleRatio target maxAbs floor)
scaleRatioNonneg _ _ _ = LTEZero

------------------------------------------------------------------------
-- Section 4: the denominator is positive
------------------------------------------------------------------------

||| Property 6: a positive floor makes the clamped denominator positive, so
||| the scaling never divides by zero.
export
denomPosWhenFloorPos : (maxAbs, floor : Nat) -> LT 0 floor -> LT 0 (maximum maxAbs floor)
denomPosWhenFloorPos maxAbs floor hf = transitive hf (maximumRightUpperBound maxAbs floor)

||| Property 7: the clamped denominator is at least maxAbs.
export
denomGeMaxAbs : (maxAbs, floor : Nat) -> LTE maxAbs (maximum maxAbs floor)
denomGeMaxAbs = maximumLeftUpperBound

||| Property 8: the clamped denominator is at least the floor.
export
denomGeFloor : (maxAbs, floor : Nat) -> LTE floor (maximum maxAbs floor)
denomGeFloor = maximumRightUpperBound

------------------------------------------------------------------------
-- Section 5: composite
------------------------------------------------------------------------

||| Main theorem: for any reservoir and any positive floor, the norm is
||| non-negative and the clamped denominator is positive and bounds both
||| the norm and the floor.
export
esnScalingPreconditionsSound : (m : Matrix) -> (maxAbs, floor : Nat) -> LT 0 floor ->
  ( LTE 0 (matrixMaxAbs m floor)
  , LT 0 (maximum maxAbs floor)
  , LTE maxAbs (maximum maxAbs floor)
  , LTE floor (maximum maxAbs floor) )
esnScalingPreconditionsSound m maxAbs floor hf =
  ( matrixMaxAbsNonneg m floor
  , denomPosWhenFloorPos maxAbs floor hf
  , denomGeMaxAbs maxAbs floor
  , denomGeFloor maxAbs floor )
