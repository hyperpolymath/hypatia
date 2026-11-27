#!/usr/bin/env bash
# shellcheck disable=SC2016 # literal backticks (AsciiDoc code spans) and inner-sh $1/$2, not expansions
# SPDX-License-Identifier: MPL-2.0
# Contracts of scripts/check-readme-claims.sh: the real tree passes, and each
# planted drift (a stale count, a dead link) turns it red. The recipe count
# must agree with and without `just` on PATH.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"
work="$(mktemp -d)"
trap 'rm -rf -- "$work"' EXIT
script="$repo_root/scripts/check-readme-claims.sh"
pass=0; fail=0

expect_msg() { # <label> <want-exit> <pattern> <command...>
  local label=$1 want=$2 pattern=$3 rc=0
  shift 3
  "$@" >"$work/out" 2>&1 || rc=$?
  if [ "$rc" -eq "$want" ] && grep -qF -- "$pattern" "$work/out"; then pass=$((pass + 1)); echo "  ok   $label"
  else fail=$((fail + 1)); echo "  FAIL $label (exit $rc, wanted $want with '$pattern')" >&2; sed 's/^/       /' "$work/out" >&2; fi
}

# A copy of the files the check reads, to plant drift in.
copy() {
  rm -rf "$work/tree"; mkdir -p "$work/tree"
  git ls-files -z README.adoc EXPLAINME.adoc docs Justfile contractile.just lib/rules lib/mix/tasks lib/hypatia/cli.ex \
    | xargs -0 -I{} sh -c 'mkdir -p "$1/$(dirname "$2")" && cp "$2" "$1/$2"' _ "$work/tree" {}
  # Every link target, so the copy resolves what the real tree resolves.
  # AsciiDoc targets are copied whole, so their anchors resolve as well.
  git ls-files | while IFS= read -r f; do
    [[ -e "$work/tree/$f" ]] && continue
    mkdir -p "$work/tree/$(dirname "$f")"
    case "$f" in *.adoc) cp "$f" "$work/tree/$f" ;; *) : > "$work/tree/$f" ;; esac
  done
}

expect_msg "the committed tree passes" 0 "match the tree" bash "$script"

copy; touch "$work/tree/lib/rules/planted_rule.ex"
expect_msg "a rule module the README does not count is refused" 1 "lib/rules is $(( $(find lib/rules -maxdepth 1 -name '*.ex' | wc -l) + 1 ))" \
  env README_CLAIMS_ROOT="$work/tree" bash "$script"

copy; sed -i 's/link:docs\/rules\/catalogue.adoc\[full rule catalogue\]/link:docs\/rules\/gone.adoc[full rule catalogue]/' "$work/tree/README.adoc"
expect_msg "a dead local link is refused" 1 "links docs/rules/gone.adoc, which does not exist" \
  env README_CLAIMS_ROOT="$work/tree" bash "$script"

copy; printf '\nplanted-recipe:\n    true\n' >> "$work/tree/Justfile"
expect_msg "a recipe the README does not count is refused" 1 "Just recipes is" \
  env README_CLAIMS_ROOT="$work/tree" bash "$script"

copy; sed -i '/^\[\[proof-ledger\]\]$/d' "$work/tree/EXPLAINME.adoc"
expect_msg "a link to an anchor the target no longer defines is refused" 1 "defines no anchor proof-ledger" \
  env README_CLAIMS_ROOT="$work/tree" bash "$script"

# A target with no explicit ID still has its section IDs. Under `set -e` a
# first grep with no match must not end the anchor list before them (#1208).
copy; printf '= Planted Title\n\n== Planted Section\n\ntext\n' > "$work/tree/docs/planted-title-only.adoc"
printf '\nlink:docs/planted-title-only.adoc#_planted_section[planted]\n' >> "$work/tree/README.adoc"
expect_msg "a section ID in a target with no explicit ID resolves" 0 "match the tree" \
  env README_CLAIMS_ROOT="$work/tree" bash "$script"
sed -i 's/#_planted_section\[/#_no_such_section[/' "$work/tree/README.adoc"
expect_msg "a missing section ID in such a target is refused" 1 "defines no anchor _no_such_section" \
  env README_CLAIMS_ROOT="$work/tree" bash "$script"

# grep's exit 2 on a target must reach the caller as "cannot be read", not be
# taken for a target that lacks the anchor (CodeRabbit on #1208). The fault is
# injected: a grep on PATH that fails for the planted file only, so the sed
# that reads the same file succeeds and cannot report the failure for it.
copy; printf '= Planted\n\n[[x]]\ntext\n' > "$work/tree/docs/planted-grep-fails.adoc"
printf '\nlink:docs/planted-grep-fails.adoc#x[planted]\n' >> "$work/tree/README.adoc"
faulty="$work/faulty"; mkdir -p "$faulty"
real_grep=$(command -v grep)
printf '#!/usr/bin/env bash\nfor a in "$@"; do [[ "$a" == *planted-grep-fails.adoc ]] && { echo "grep: $a: I/O error" >&2; exit 2; }; done\nexec %q "$@"\n' "$real_grep" > "$faulty/grep"
chmod +x "$faulty/grep"
expect_msg "a target grep cannot read is refused as unreadable" 1 "the anchors of docs/planted-grep-fails.adoc cannot be read" \
  env README_CLAIMS_ROOT="$work/tree" PATH="$faulty:$PATH" bash "$script"

copy; touch "$work/tree/lib/mix/tasks/hypatia.planted.ex"
expect_msg "a Mix task the README does not count is refused" 1 "Mix tasks" \
  env README_CLAIMS_ROOT="$work/tree" bash "$script"

# The recipe count must be the same with just and with the grep fallback, so
# a Justfile shape the fallback misreads cannot pass. CI installs just for this.
if ! command -v just >/dev/null 2>&1; then
  fail=$((fail + 1)); echo "  FAIL just is not on PATH, so the recipe-count parity check cannot run" >&2
else
  nojust="$work/nojust"; mkdir -p "$nojust"
  for t in bash env find grep sed wc sort tr dirname cat mktemp rm mkdir cp git xargs sh touch printf; do
    p=$(command -v "$t") && ln -sf "$p" "$nojust/$t"
  done
  expect_msg "without just the grep count gives the same recipes" 0 "match the tree" \
    env PATH="$nojust" bash "$script"
fi

echo "readme-claims-test: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
