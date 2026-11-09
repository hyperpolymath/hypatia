#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Contracts of scripts/proof-ledger.sh: the real ledger passes, and every
# planted defect (an overclaim, a vocabulary escape, a renamed proof, a
# non-canonical line, a hand-edited table) turns it red.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"
work="$(mktemp -d)"
trap 'rm -rf -- "$work"' EXIT
script="scripts/proof-ledger.sh"
pass=0; fail=0

expect() { # <label> <want-exit> <command...>
  local label=$1 want=$2 rc=0
  shift 2
  "$@" >"$work/out" 2>&1 || rc=$?
  if [ "$rc" -eq "$want" ]; then pass=$((pass + 1)); echo "  ok   $label"
  else fail=$((fail + 1)); echo "  FAIL $label (exit $rc, wanted $want)" >&2; sed 's/^/       /' "$work/out" >&2; fi
}

# mutate <name> <jq filter applied to the row whose id is $id> <id>
mutate() {
  jq -cS --arg id "$3" "if .id == \$id then $2 else . end" verification/proof-ledger.jsonl > "$work/$1.jsonl"
}

expect "the committed ledger is valid" 0 bash "$script" check
expect "the committed PROOF-STATUS block is the ledger's rendering" 0 bash "$script" render --check

mutate status-escape '.status = "COMPLETE"' abi-types
expect "a status outside the closed vocabulary is refused" 1 env PROOF_LEDGER="$work/status-escape.jsonl" bash "$script" check

mutate hidden-axioms '.status = "proven"' lean-rate-limiting
expect "proven with axioms listed is refused (it is proven-with-postulates)" 1 env PROOF_LEDGER="$work/hidden-axioms.jsonl" bash "$script" check

mutate unnamed-axioms '.axioms = []' lean-parser-totality
expect "proven-with-postulates without its axioms is refused" 1 env PROOF_LEDGER="$work/unnamed-axioms.jsonl" bash "$script" check

mutate renamed-proof '.names += ["noSuchTheorem"]' safety-triangle
expect "a name the file does not hold is refused" 1 env PROOF_LEDGER="$work/renamed-proof.jsonl" bash "$script" check

mutate missing-file '.file = "verification/proofs/idris2/Gone.idr"' quarantine
expect "a file that does not exist is refused" 1 env PROOF_LEDGER="$work/missing-file.jsonl" bash "$script" check

mutate unbounded-test '.bounds = ""' kin-gate
expect "tested-only without bounds is refused" 1 env PROOF_LEDGER="$work/unbounded-test.jsonl" bash "$script" check

mutate open-graded '.pwg = "3"' pagerank-convergence
expect "a PWG on an open theory row is refused" 1 env PROOF_LEDGER="$work/open-graded.jsonl" bash "$script" check

mutate grade-without-leg '.pwg = "2i"' logtalk-migration
expect "PWG-2i with no counted recheck leg is refused" 1 env PROOF_LEDGER="$work/grade-without-leg.jsonl" bash "$script" check

mutate weak-agda '.checker = "Agda 2.8.0.2, --safe"' logtalk-migration
expect "a PWG without the PROOF-POLICY 2.1 invocation is refused" 1 env PROOF_LEDGER="$work/weak-agda.jsonl" bash "$script" check

mutate engineering-grade '.pwg = "3"' abi-ffi
expect "a PWG on an engineering row is refused" 1 env PROOF_LEDGER="$work/engineering-grade.jsonl" bash "$script" check

head -1 verification/proof-ledger.jsonl | jq . > "$work/pretty.jsonl"
tail -n +2 verification/proof-ledger.jsonl >> "$work/pretty.jsonl"
expect "a line not in canonical (JCS) form is refused" 1 env PROOF_LEDGER="$work/pretty.jsonl" bash "$script" check

{ cat verification/proof-ledger.jsonl; head -1 verification/proof-ledger.jsonl; } > "$work/dup.jsonl"
expect "a duplicate id is refused" 1 env PROOF_LEDGER="$work/dup.jsonl" bash "$script" check

cp verification/PROOF-STATUS.adoc "$work/doc.adoc"
sed -i '0,/^|proven$/s//|proven-with-postulates/' "$work/doc.adoc"
expect "a hand edit to the rendered table is refused" 1 env PROOF_STATUS_DOC="$work/doc.adoc" bash "$script" render --check

echo "proof-ledger-test: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
