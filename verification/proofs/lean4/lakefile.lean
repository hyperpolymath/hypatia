-- SPDX-License-Identifier: PMPL-1.0-or-later
import Lake
open Lake DSL

package hypatia_proofs where
  name := `hypatia_proofs

lean_lib HypatiaProofs where
  roots := #[`BayesianUpdate, `RateLimiting, `ParserTotality, `PageRankInvariants, `ESNSpectralScaling]
