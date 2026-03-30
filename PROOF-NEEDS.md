# PROOF-NEEDS.md — hypatia

## Current State

- **src/abi/*.idr**: YES — `Types.idr`, `FFI.idr`, `GraphQL.idr`, `GRPC.idr`, `REST.idr`
- **Dangerous patterns**: 0 in own code (32 references are all in rule definitions that *detect* dangerous patterns in other repos)
- **LOC**: ~144,000 (Rust + Elixir + Idris2)
- **ABI layer**: Comprehensive Idris2 ABI definitions

## What Needs Proving

| Component | What | Why |
|-----------|------|-----|
| Rule engine correctness | Rules produce consistent, deterministic findings | False positives/negatives undermine CI trust |
| Finding severity classification | Severity assignment is monotonic and consistent | Incorrect severity leads to wrong triage decisions |
| Scanner composition | Multiple scanners compose without conflicting findings | Overlapping scan results must merge correctly |
| Neurosymbolic confidence scores | Confidence metric is bounded [0,1] and monotonic | Unbounded scores break decision thresholds |
| Triangle router | Routing decisions are deterministic and complete | Messages must never be dropped or misrouted |
| VeriSimDB connector | Data integrity between scan and storage | Findings must not be corrupted in transit |

## Recommended Prover

**Idris2** — ABI layer already extensive in Idris2. Rule correctness and confidence bounds are natural fits for dependent types. Scanner composition proofs could use algebraic properties.

## Priority

**HIGH** — Hypatia is the neurosymbolic CI/CD intelligence platform used across all repos. Incorrect findings propagate errors to every downstream project. Rule engine correctness and confidence score bounds are critical.
