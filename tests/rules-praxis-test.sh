#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Contracts of hypatia_rules_praxis.deed (standards A5 rule registry):
#   * the committed file equals a fresh run of scripts/gen-rules-praxis.sh;
#   * the generator is idempotent (two runs, equal bytes; A5 GEN1/GEN2);
#   * no :id appears twice (A5 RL1), none contains `*` (RL5), each fits the
#     A5 3.1 grammar in the `hypatia` domain and has at most 200 characters;
#   * every :supersedes / :superseded-by names an id in the file, both ways;
#   * the tombstoned ids are kept and `:status retired`;
#   * with DEED_LINT=<path to standards deed_lint.js>, the deed lints clean.
# Each invariant is also shown to fail on a planted copy (negative control).
set -euo pipefail
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"
deed=hypatia_rules_praxis.deed
work="$(mktemp -d)"
trap 'rm -rf -- "$work"' EXIT
pass=0; fail=0
ok() { pass=$((pass + 1)); echo "  ok   $1"; }
bad() { fail=$((fail + 1)); echo "  FAIL $1" >&2; }

# Prints one line per broken invariant of the registry file $1.
invariants() {
  local f=$1
  grep -oE '^ *:id "[^"]*"' "$f" | sed -E 's/^ *:id "(.*)"$/\1/' >"$work/ids"
  sort "$work/ids" | uniq -d | sed 's/^/duplicate id: /'
  grep -F '*' "$work/ids" | sed 's/^/wildcard id: /' || true
  grep -vE '^hypatia/[A-Za-z0-9._-]+(/[A-Za-z0-9._-]+)*$' "$work/ids" | sed 's/^/bad id: /' || true
  awk 'length > 200 { print "id over 200 characters: " $0 }' "$work/ids"
  # A5 RL3/R08: no two rows share a text.
  grep -E '^ *:text "' "$f" | sort | uniq -d | sed 's/^ */duplicate text: /' 
  # Supersession pointers resolve, and each pair points both ways.
  awk '
    /^ *:id "/ { id = $0; sub(/^ *:id "/, "", id); sub(/"$/, "", id) }
    /^ *:superseded-by "/ { t = $0; sub(/^ *:superseded-by "/, "", t); sub(/"$/, "", t); by[id] = t }
    /^ *:supersedes \(/ { s = $0; sub(/^ *:supersedes \(/, "", s); sub(/\)$/, "", s)
      n = split(s, a, " "); for (i = 1; i <= n; i++) { gsub(/"/, "", a[i]); sup[id, a[i]] = 1; old[a[i]] = id } }
    /^ *:id "/ { ids[id] = 1 }
    END {
      for (o in by) { if (!(by[o] in ids)) print "superseded-by does not resolve: " o " -> " by[o]
                      else if (!((by[o], o) in sup)) print "no :supersedes back-pointer: " by[o] " for " o }
      for (o in old) { if (!(o in ids)) print "supersedes does not resolve: " old[o] " -> " o
                       else if (by[o] != old[o]) print "no :superseded-by back-pointer: " o " for " old[o] }
    }' "$f"
  for t in hypatia/structural_drift/SD012 hypatia/dogfooding/HYP-DOG-002; do
    awk -v t="$t" '/^ *:id "/ { cur = $0 } cur ~ ("\"" t "\"") && /:status retired\)/ { f = 1 } END { exit !f }' "$f" ||
      echo "tombstone not retired: $t"
  done
}

expect_clean() { # <label> <file>
  invariants "$2" >"$work/out"
  if [ -s "$work/out" ]; then bad "$1"; sed 's/^/       /' "$work/out" >&2; else ok "$1"; fi
}
expect_broken() { # <label> <file> <message fragment>
  invariants "$2" >"$work/out"
  if grep -qF -- "$3" "$work/out"; then ok "$1"; else bad "$1 (wanted '$3')"; cat "$work/out" >&2; fi
}

if bash scripts/gen-rules-praxis.sh --check >"$work/out" 2>&1; then ok "committed $deed matches the code"
else bad "committed $deed is stale"; sed 's/^/       /' "$work/out" >&2; fi

bash scripts/gen-rules-praxis.sh --stdout >"$work/a"
bash scripts/gen-rules-praxis.sh --stdout >"$work/b"
if cmp -s "$work/a" "$work/b"; then ok "two generations are byte-equal"; else bad "generation is not deterministic"; fi

expect_clean "the committed registry keeps every invariant" "$deed"

# Negative controls: each planted fault is caught.
first=$(grep -m1 -oE ':id "[^"]*"' "$deed")
second=$(grep -oE ':id "[^"]*"' "$deed" | sed -n 2p)
sed "s|$second|$first|" "$deed" >"$work/dup.deed"
expect_broken "a duplicated id is refused" "$work/dup.deed" "duplicate id:"
grep -m1 -E '^ *:text "' "$deed" >"$work/t1"
t1=$(sed -E 's/^ *//' "$work/t1")
awk -v t="$t1" 'BEGIN{n=0} /^ *:text "/ { n++; if (n == 2) { print "      " t; next } } { print }' "$deed" >"$work/duptext.deed"
expect_broken "a shared text is refused" "$work/duptext.deed" "duplicate text:"
sed "0,\\|$first|s|$first|:id \"hypatia/code_safety/*\"|" "$deed" >"$work/wild.deed"
expect_broken "a wildcard id is refused" "$work/wild.deed" "wildcard id:"
sed '0,/:superseded-by "/s/:superseded-by "/:superseded-by "hypatia\/gone\//' "$deed" >"$work/dangle.deed"
expect_broken "a dangling :superseded-by is refused" "$work/dangle.deed" "superseded-by does not resolve"
awk '/:id "hypatia\/structural_drift\/SD012"/ { f = 1 } f && /:status retired\)/ { sub(/retired/, "active"); f = 0 } { print }' "$deed" >"$work/tomb.deed"
expect_broken "an un-retired tombstone is refused" "$work/tomb.deed" "tombstone not retired: hypatia/structural_drift/SD012"

# Facts the code states, read back from the registry (review of #1070): a
# provenance table is not a severity table, `severity: :advisory` is advisory,
# and a rule that fires at several severities keeps the highest.
row() { awk -v t="\"$1\"" '$1 == ":id" { on = ($2 == t) } on { print }' "$deed"; } # one rule-def
if row hypatia/branch_protection/BP001 | grep -q ':severity high' && row hypatia/supply_chain/SC001 | grep -q ':status active'; then ok "opt-in scanner rules are registered"; else bad "BP001/SC001 missing or wrong"; fi
if row hypatia/git_state/GS007 | grep -q ':class advisory'; then ok "an advisory-severity rule is class advisory"; else bad "GS007 is not class advisory"; fi
if row hypatia/code_scanning_alerts/CSA006 | grep -q ':severity medium'; then ok "a multi-severity rule keeps its highest severity"; else bad "CSA006 is not medium"; fi

if row hypatia/pin_integrity/PI005 | grep -q ':class advisory' && row hypatia/disambiguation_rules/DR001 | grep -q ':class advisory'; then ok "rules the code calls advisory in words are class advisory"; else bad "PI005 or DR001 is not class advisory"; fi

if row hypatia/workflow_audit/duplicate_workflow | grep -q ':severity minor' && row hypatia/cicd_rules/waste_duplicate_workflow | grep -q ':severity medium'; then ok "named atom rules carry the severity the code states"; else bad "duplicate_workflow rows lack their severity"; fi

# Severities the code computes (an if expression, a helper): the highest branch.
# A severity read from run-time alert data (CSA001) has none to record.
if row hypatia/supply_chain/SC012 | grep -q ':severity high' && row hypatia/structural_drift/SD008 | grep -q ':severity critical' \
  && row hypatia/research_extensions/RE001 | grep -q ':severity high' && ! row hypatia/code_scanning_alerts/CSA001 | grep -q ':severity'; then
  ok "computed severities are read; run-time severities stay empty"; else bad "computed severity rows are wrong"; fi

# The names findings actually carry resolve (A5 R09). Ground truth: every
# rule_module/type pair in this repo's .hypatia-baseline.json, and the 17 pairs
# A5 counts in the standards baseline. A baseline entry whose name no code in
# lib/ mentions is stale, not a gap; the list below is checked against lib/ so it
# cannot hide a real one.
STALE='inbox_automation/IA001 inbox_automation/IA002 inbox_automation/IA003 inbox_automation/IA004 inbox_automation/IA005 inbox_automation/IA006 inbox_automation/IA007 workflow_audit/inbox_steward_intake_missing workflow_audit/inbox_steward_missing'
STANDARDS='cicd_rules/banned_language_file code_safety/js_exec_sync code_safety/ncl_k9_missing_pedigree code_safety/unwrap_without_check code_scanning_alerts/CSA001 code_scanning_alerts/CSA002 code_scanning_alerts/CSA003 content_patterns/download_then_run_shell content_patterns/eval_in_shell content_patterns/http_in_docs content_patterns/known_fake_action_sha content_patterns/npx_in_workflow green_web/non_green_hosting research_extensions/RE001 scorecard/DependencyPinning structural_drift/SD022 workflow_hardening/WH013'
pairs_missing() { # <deed file>: baseline pairs with no registry id
  local pair out=""
  for pair in $(jq -r '.[] | "\(.rule_module)/\(.type)"' .hypatia-baseline.json | sort -u) $STANDARDS; do
    grep -qF -- "\"hypatia/$pair\"" "$1" && continue
    case " $STALE " in *" $pair "*) continue ;; esac
    out="$out $pair"
  done
  printf '%s' "$out"
}
unresolved=$(pairs_missing "$deed")
if [ -z "$unresolved" ]; then ok "every baseline pair resolves to a registry id"; else bad "baseline pairs not in the registry:$unresolved"; fi
sed 's|"hypatia/code_safety/js_exec_sync"|"hypatia/code_safety/js_exec_gone"|' "$deed" >"$work/pair.deed"
if [ "$(pairs_missing "$work/pair.deed")" = " code_safety/js_exec_sync" ]; then ok "a baseline pair missing from the registry is caught"; else bad "a removed baseline pair was not caught"; fi
stale_wrong=""
for pair in $STALE; do
  if grep -rqF -- "${pair#*/}" lib; then stale_wrong="$stale_wrong $pair"; fi
done
if [ -z "$stale_wrong" ]; then ok "the stale baseline names are in no code under lib/"; else bad "listed as stale but lib/ mentions:$stale_wrong"; fi

# Coverage of the normalizers: every `rule_module: "m"` whose `type: "t"` is a
# string literal a few lines below (cli.ex and the rule files) is in the registry.
# A name a finding is moved off by a `rule_id` override is not (WF029 is emitted
# under ci_failure_triage), and the module-owned names below are checked by hand.
missing_pairs=""
while read -r pair; do
  [ -z "$pair" ] && continue
  grep -qF -- "\"hypatia/$pair\"" "$deed" || missing_pairs="$missing_pairs $pair"
done < <(awk '
  /rule_module: "[a-z_]+"/ { match($0, /rule_module: "[a-z_]+"/); m = substr($0, RSTART + 14, RLENGTH - 15); left = 6; next }
  left > 0 { left--; if (match($0, /type: "[A-Za-z0-9_]+"/)) { print m "/" substr($0, RSTART + 7, RLENGTH - 8); left = 0 } }
' lib/hypatia/cli.ex lib/rules/*.ex | sort -u)
if [ -z "$missing_pairs" ]; then ok "every literal rule_module/type pair in the code is in the registry"; else bad "pairs in the code but not the registry:$missing_pairs"; fi
if ! grep -qF '"hypatia/workflow_audit/always_after_cancel"' "$deed" && grep -qF '"hypatia/ci_failure_triage/always_after_cancel"' "$deed"; then ok "a name moved by a rule_id override is registered under its real module only"; else bad "always_after_cancel is registered under workflow_audit, or missing under ci_failure_triage"; fi
if row hypatia/structured_format/SF001 | grep -q ':class advisory' && row hypatia/cicd_rules/banned_language_file | grep -q ':severity critical' && row hypatia/implementation_inside_canon/HYP-S009 | grep -q ':status active' && row hypatia/workflow_hardening/invalid_actions_lock | grep -q ':status active'; then ok "fixed names carry the code's severity and advisory flag; HYP-S009 and invalid_actions_lock are registered"; else bad "a fixed-name row is wrong"; fi

# Portability: gawk and mawk differ (a `\[` in a -v value is valid in one and a
# fatal regex error in the other, and the awk then dies without a word inside a
# process substitution). The output must not depend on which awk is installed, so
# run the generator under each one found and require the committed bytes and an
# empty stderr.
awkdir="$work/awk"; mkdir -p "$awkdir"
for a in gawk mawk original-awk; do
  command -v "$a" >/dev/null 2>&1 || continue
  ln -sf "$(command -v "$a")" "$awkdir/awk"
  # SIGPIPE is ignored here, as it is for a step on the CI runner: a writer to a
  # closed pipe then prints "Broken pipe" instead of dying silently, so an
  # early-closing pipe (`head -1`) in the generator shows up on a dev machine too.
  if PATH="$awkdir:$PATH" sh -c 'trap "" PIPE; exec bash scripts/gen-rules-praxis.sh --stdout' 2>"$work/awk.err" | cmp -s - "$deed" && [ ! -s "$work/awk.err" ]; then
    ok "the generator gives the committed registry under $a"
  else
    bad "the generator differs or warns under $a"; head -3 "$work/awk.err" >&2
  fi
done

if [ -n "${DEED_LINT:-}" ]; then
  if node "$DEED_LINT" "$deed" >"$work/out" 2>&1; then ok "deed_lint.js accepts $deed"
  else bad "deed_lint.js rejects $deed"; sed 's/^/       /' "$work/out" >&2; fi
else
  echo "  skip deed_lint.js (set DEED_LINT to the standards linter)"
fi

echo "rules-praxis: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
