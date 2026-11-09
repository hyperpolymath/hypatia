#!/usr/bin/env bash
# shellcheck disable=SC2016 # literal backticks (AsciiDoc code spans) and inner-sh $1/$2, not expansions
# SPDX-License-Identifier: MPL-2.0
# The structural counts the README quotes, measured from the tree, and every
# local link: target in the README, EXPLAINME and the docs index. A count or a
# link that no longer matches fails the check, so the README cannot drift the
# way it did (35 rule modules quoted against 42 on disk).
#
# Usage: check-readme-claims.sh            # check, exit 1 on any mismatch
#        README_CLAIMS_ROOT=DIR ...        # check another checkout (tests)
set -euo pipefail
root=${README_CLAIMS_ROOT:-$(cd "$(dirname "$0")/.." && pwd)}
cd "$root"
readme=README.adoc
fail=0
say() { echo "::error::readme-claims: $*"; fail=1; }

words=(zero one two three four five six seven eight nine ten eleven twelve
       thirteen fourteen fifteen sixteen seventeen eighteen nineteen twenty)

# The Just recipes `just --summary` lists: every public recipe in the Justfile
# and the files it imports. Without just, the same rule as a grep.
recipes() {
  if command -v just >/dev/null 2>&1; then
    just --summary | wc -w
  else
    local files=(Justfile)
    while IFS= read -r f; do [ -f "$f" ] && files+=("$f"); done \
      < <(sed -nE 's/^import\??[[:space:]]+"([^"]+)".*/\1/p' Justfile)
    grep -hE '^@?[A-Za-z][A-Za-z0-9_-]*( [^:]*)?:( |$)' "${files[@]}" | grep -vc ':='
  fi
}

# expect <what> <measured> <pattern with N for the number>
expect() {
  local what=$1 n=$2 pat=${3//N/$2}
  grep -qF -- "$pat" "$readme" || say "$what is $n in the tree; $readme should say: $pat"
}

rules=$(find lib/rules -maxdepth 1 -name '*.ex' | wc -l)
expect "lib/rules" "$rules" "*Symbolic reasoning* — N Elixir rule modules"
expect "lib/rules" "$rules" '| `lib/rules/` | N rule modules'
expect "lib/" "$(find lib -name '*.ex' | wc -l)" '| `lib/` | N Elixir source files'
expect "Just recipes" "$(recipes)" '`just --list` shows all N recipes'

tasks=$(find lib/mix/tasks -maxdepth 1 -name 'hypatia.*.ex' | wc -l)
named=$(grep -A2 '^Mix tasks cover the operational surface' "$readme" | grep -oE '`mix hypatia\.[a-z_]+`' | sort -u | wc -l)
more=$((tasks - named))
if [ "$more" -ge 0 ] && [ "$more" -lt ${#words[@]} ]; then
  expect "Mix tasks ($tasks, $named named)" "${words[$more]}" "and N more."
else
  say "Mix tasks: $tasks in the tree, $named named in $readme"
fi

# The escript's top-level commands: the case arms of Hypatia.CLI.main/1.
commands=$(grep -cE '^[[:space:]]+\["[a-z-]+"( \| [a-z_]+)?\] ->' lib/hypatia/cli.ex)
if [ "$commands" -lt ${#words[@]} ]; then
  expect "CLI commands" "${words[$commands]}" "The escript exposes N commands:"
else
  say "CLI commands: $commands"
fi

# Every local link: target resolves, relative to the document that names it.
for doc in "$readme" EXPLAINME.adoc docs/README.adoc; do
  [ -f "$doc" ] || continue
  dir=$(dirname "$doc")
  while IFS= read -r target; do
    case "$target" in http://*|https://*|mailto:*|"") continue ;; esac
    path=${target%%#*}
    [ -z "$path" ] && continue
    [ -e "$dir/$path" ] || say "$doc links $target, which does not exist"
  done < <(grep -oE 'link:[^[ ]+\[' "$doc" | sed -E 's/^link:(.*)\[$/\1/')
done

[ "$fail" -eq 0 ] && echo "readme-claims: counts and local links match the tree"
exit "$fail"
