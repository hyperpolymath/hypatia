-- SPDX-License-Identifier: MPL-2.0
import Lake
open Lake DSL

package hypatia_proofs

-- @[default_target] so a bare `lake build` actually compiles (and thus
-- type-checks) the proof library. Without it, `lake build` reports success
-- having built 0 targets — the verify-proofs.yml gate would pass vacuously.
@[default_target]
lean_lib HypatiaProofs where
  roots := #[`BayesianUpdate, `RateLimiting, `ParserTotality, `PageRankInvariants, `ESNSpectralScaling]
