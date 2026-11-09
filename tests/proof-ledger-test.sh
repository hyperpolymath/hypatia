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


# expect_msg <label> <want-exit> <pattern> <command...>: as expect, and the
# output must name the rule (so another rule failing first cannot pass it).
expect_msg() {
  local label=$1 want=$2 pattern=$3 rc=0
  shift 3
  "$@" >"$work/out" 2>&1 || rc=$?
  if [ "$rc" -eq "$want" ] && grep -qF -- "$pattern" "$work/out"; then pass=$((pass + 1)); echo "  ok   $label"
  else fail=$((fail + 1)); echo "  FAIL $label (exit $rc, wanted $want with '$pattern')" >&2; sed 's/^/       /' "$work/out" >&2; fi
}
# mutate <name> <jq filter applied to the row whose id is $id> <id>
mutate() {
  # A mutant whose target row is gone would mutate nothing and pass for the
  # wrong reason: refuse it.
  if ! jq -e --arg id "$3" "select(.id == \$id)" verification/proof-ledger.jsonl >/dev/null; then
    fail=$((fail + 1)); echo "  FAIL mutant $1 targets row $3, which the ledger does not have" >&2
  fi
  jq -cS --arg id "$3" "if .id == \$id then $2 else . end" verification/proof-ledger.jsonl > "$work/$1.jsonl"
}

expect "the committed ledger is valid" 0 bash "$script" check
expect "the committed PROOF-STATUS block is the ledger's rendering" 0 bash "$script" render --check

mutate status-escape '.status = "COMPLETE"' abi-types
expect "a status outside the closed vocabulary is refused" 1 env PROOF_LEDGER="$work/status-escape.jsonl" bash "$script" check

mutate hidden-axioms '.axioms = ["propext"]' rate-limiting
expect "proven with axioms listed is refused (it is proven-with-postulates)" 1 env PROOF_LEDGER="$work/hidden-axioms.jsonl" bash "$script" check

mutate unnamed-axioms '.status = "proven-with-postulates"' parser-totality
expect "proven-with-postulates without its axioms is refused" 1 env PROOF_LEDGER="$work/unnamed-axioms.jsonl" bash "$script" check

mutate renamed-proof '.names += ["noSuchTheorem"]' safety-triangle
expect "a name the file does not hold is refused" 1 env PROOF_LEDGER="$work/renamed-proof.jsonl" bash "$script" check

mutate missing-file '.file = "verification/proofs/idris2/Gone.idr"' quarantine
expect "a file that does not exist is refused" 1 env PROOF_LEDGER="$work/missing-file.jsonl" bash "$script" check

mutate unbounded-test '.bounds = ""' kin-gate
expect "tested-only without bounds is refused" 1 env PROOF_LEDGER="$work/unbounded-test.jsonl" bash "$script" check

mutate open-graded '.pwg = "3"' pagerank-convergence
expect_msg "a PWG on an open theory row is refused" 1 "PWG-3 needs an Agda proof that is proven" env PROOF_LEDGER="$work/open-graded.jsonl" bash "$script" check

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

expect "every census the claim documents quote is the ledger's" 0 bash "$script" claims

# Built from the ledger's own census, so the contracts outlive any count.
census_now=$(bash "$script" check | sed -n 's/^proof-ledger: [0-9]* rows valid; \(.*\) Theory rows:.*/\1/p')
proven_now=${census_now%% proven of*}
printf 'Ledger: %s\n' "$((proven_now + 1))${census_now#"$proven_now"}" > "$work/wrong.adoc"
expect "a census that disagrees with the ledger is refused" 1 env PROOF_CLAIM_DOCS="$work/wrong.adoc" bash "$script" claims

printf 'We have %s so far.\n' "${census_now%%;*}" > "$work/short.adoc"
expect "a bare count without the rest of the census is refused" 1 env PROOF_CLAIM_DOCS="$work/short.adoc" bash "$script" claims

printf 'Ledger: %s\n' "$census_now" | sed 's/ proven of / proven of\n/; s/, \([0-9]* argued\)/,\n\1/' > "$work/wrapped.adoc"
expect "a census wrapped across lines is read whole" 0 env PROOF_CLAIM_DOCS="$work/wrapped.adoc" bash "$script" claims

head -c -1 verification/proof-ledger.jsonl > "$work/no-lf.jsonl"
expect_msg "a ledger without its final LF is refused" 1 "must end in exactly one LF" env PROOF_LEDGER="$work/no-lf.jsonl" bash "$script" check

{ cat verification/proof-ledger.jsonl; echo; } > "$work/extra-lf.jsonl"
expect_msg "a ledger with an extra final LF is refused" 1 "must end in exactly one LF" env PROOF_LEDGER="$work/extra-lf.jsonl" bash "$script" check

mutate bad-type '.names = "connectorCount"' abi-types
expect_msg "a mistyped field is reported (exit 1), not a jq crash" 1 "field names must be a array" env PROOF_LEDGER="$work/bad-type.jsonl" bash "$script" check
if grep -q "jq: error" "$work/out"; then fail=$((fail + 1)); echo "  FAIL the mistyped row crashed jq" >&2; else pass=$((pass + 1)); echo "  ok   the mistyped row does not crash jq"; fi

{ echo '{"id": oops'; cat verification/proof-ledger.jsonl; } > "$work/not-json.jsonl"
expect_msg "a line that is not JSON is named, and the run stops there" 1 "line 1 is not JSON" env PROOF_LEDGER="$work/not-json.jsonl" bash "$script" check

grep -v '^// END GENERATED: proof ledger$' verification/PROOF-STATUS.adoc > "$work/no-marker.adoc"
expect_msg "render --check with a marker missing says to restore it (exit 2)" 2 "lacks the generated-block markers" env PROOF_STATUS_DOC="$work/no-marker.adoc" bash "$script" render --check

echo "proof-ledger-test: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
