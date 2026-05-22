-- SPDX-License-Identifier: MPL-2.0
import Lake
open Lake DSL

package hypatia_proofs where
  name := `hypatia_proofs

lean_lib HypatiaProofs where
  roots := #[`BayesianUpdate, `RateLimiting, `ParserTotality, `PageRankInvariants, `ESNSpectralScaling]
