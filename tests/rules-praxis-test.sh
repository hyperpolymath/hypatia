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

if [ -n "${DEED_LINT:-}" ]; then
  if node "$DEED_LINT" "$deed" >"$work/out" 2>&1; then ok "deed_lint.js accepts $deed"
  else bad "deed_lint.js rejects $deed"; sed 's/^/       /' "$work/out" >&2; fi
else
  echo "  skip deed_lint.js (set DEED_LINT to the standards linter)"
fi

echo "rules-praxis: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
