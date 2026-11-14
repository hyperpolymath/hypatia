#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# check-sq-map.sh: does every hypatia/ci_failure_triage id this tree emits, and
# each of the two ADR 0004 workflow_audit ids, have a row in the Hypatia to
# squabbler rule map, proven/registry/sq-map.tsv in
# hyperpolymath/proven-cicd-and-workflows?
#
#   scripts/check-sq-map.sh PROVEN_CICD_DIR [DEED]
#
# PROVEN_CICD_DIR is a checkout of proven-cicd-and-workflows (CI checks out its
# main branch, because the question is whether the rule is mapped now).
#
# DEED defaults to a fresh run of `scripts/gen-rules-praxis.sh --stdout`: the
# registry this tree's rule sources emit now, so a rule added without
# regenerating hypatia_rules_praxis.deed is still seen here. Whether the
# committed deed is stale is a separate check (quality.yml, "Rule registry
# (hypatia_rules_praxis.deed) matches the code and lints"). Pass a DEED to check
# that file instead.
#
# The verdict is proven-cicd's own tests/registry-sq-map.sh at its main branch,
# run with HYPATIA_PRAXIS=DEED and HYPATIA_PRAXIS_SOURCE=1 (DEED is this
# change's registry, not proven-cicd's pinned one, so the map's praxis column,
# which describes the pin, may disagree with it: a WARN there, not a FAIL; a
# missing row still fails). This script adds only the cure: the exact row to add
# for each id the test names, and the order that keeps both repositories green:
#   1. a proven-cicd PR adds the row PENDING (`unmapped`, praxis `absent`); it
#      passes there against proven-cicd's Hypatia pin, which lacks the rule;
#   2. re-run this job: the row exists, so it passes;
#   3. after this change merges, proven-cicd flips the column to `present` when
#      it moves its Hypatia pin (its weekly run warns until then).
# A row added `present` in step 1 would fail proven-cicd's own check.
#
# Exit: 0 every required id has a row; 1 the map lacks a row or a row is wrong;
# 2 NOT RUN (the checkout, the generator or the test is missing or failed).
# CI fails on 2 as well as on 1: a check that did not run is never a pass.
set -euo pipefail
cd "$(dirname "$0")/.."

summary=${GITHUB_STEP_SUMMARY:-/dev/null}
notrun() {
  echo "::error title=SQ map check NOT RUN::$*"
  printf '### SQ map check: NOT RUN\n\n%s\n\nA check that did not run is not a pass.\n' "$*" >>"$summary"
  exit 2
}

proven=${1:-}
[[ -n $proven ]] || notrun "usage: scripts/check-sq-map.sh PROVEN_CICD_DIR [DEED]"
test=$proven/tests/registry-sq-map.sh
map=$proven/proven/registry/sq-map.tsv
[[ -f $test && -f $map ]] ||
  notrun "$proven has no tests/registry-sq-map.sh or proven/registry/sq-map.tsv (a failed or partial checkout of proven-cicd-and-workflows)"

work=$(mktemp -d)
trap 'rm -rf -- "$work"' EXIT
if [[ -n ${2:-} ]]; then
  deed=$2
  [[ -f $deed ]] || notrun "$deed missing"
  what="$deed"
else
  deed=$work/hypatia_rules_praxis.deed
  bash scripts/gen-rules-praxis.sh --stdout >"$deed" || notrun "scripts/gen-rules-praxis.sh --stdout failed"
  [[ -s $deed ]] || notrun "scripts/gen-rules-praxis.sh --stdout printed nothing"
  what="the registry the rule sources emit now (scripts/gen-rules-praxis.sh --stdout)"
fi
echo "checking $what against $map"

# SQ_MAP and SQUABBLER_RULES are the test's other inputs: unset, so it reads
# the checkout's own map and judges only the Hypatia direction.
rc=0
env -u SQ_MAP -u SQUABBLER_RULES HYPATIA_PRAXIS="$deed" HYPATIA_PRAXIS_SOURCE=1 bash "$test" >"$work/out" 2>&1 || rc=$?
cat "$work/out"
case $rc in
  0) printf '### SQ map check: every required id has a row\n\nChecked %s.\n' "$what" >>"$summary"; exit 0 ;;
  1) grep -q '^FAIL ' "$work/out" ||
       notrun "proven-cicd tests/registry-sq-map.sh exited 1 without naming a broken rule (it could not judge)" ;;
  *) notrun "proven-cicd tests/registry-sq-map.sh exited $rc" ;;
esac

# The test failed. Name the cure for each line it printed.
note="in the praxis registry, not in ADR0004 mapping table; SQ rule is the squabbler owner's decision"
missing=$(sed -n 's/^FAIL triage ids in the registry with no row[^:]*: //p' "$work/out" | tr ' ' '\n' | grep . || true)
{
  echo "### SQ map check: the map in proven-cicd-and-workflows needs a change"
  echo
  echo "In this order, so that neither repository merges red:"
  echo
  echo "1. Add the row(s) below to \`proven/registry/sq-map.tsv\` in a pull request on"
  echo "   hyperpolymath/proven-cicd-and-workflows, with praxis \`absent\` (a pending row: the"
  echo "   rule is not in proven-cicd's Hypatia pin yet, and \`present\` would fail there)."
  echo "   Keep the rows in id order and the counts in \`proven/registry/README.adoc\` true."
  echo "2. Once that is merged, re-run this job here: it reads proven-cicd at main."
  echo "3. After this change merges, proven-cicd flips the column to \`present\` when it"
  echo "   moves its Hypatia pin; its weekly run warns until then."
  echo
} >"$work/cure"
if [[ -n $missing ]]; then
  echo "Rows to add (tab-separated: a5_id, sq_id, status, praxis, note):" >>"$work/cure"
  echo '```' >>"$work/cure"
  while IFS= read -r id; do
    if [[ $id == hypatia/ci_failure_triage/* ]]; then
      printf '%s\tunassigned\tunmapped\tabsent\t%s\n' "$id" "$note" >>"$work/cure"
    else
      # The ADR 0004 workflow_audit ids are mapped rows; one is missing only
      # when someone deleted it. Its SQ id comes from ADR 0004, not from here.
      printf '%s\t<SQ id from ADR 0004>\tmapped\tpresent\tADR0004 mapping table\n' "$id" >>"$work/cure"
    fi
  done <<<"$missing"
  echo '```' >>"$work/cure"
  echo >>"$work/cure"
  echo "\`unmapped\` with \`unassigned\` is right until the squabbler owner gives the id an SQ rule." >>"$work/cure"
  echo "A deleted ADR 0004 \`mapped\` row goes back as it was (its id is in proven-cicd's pin, so \`present\`)." >>"$work/cure"
fi
other=$(grep '^FAIL ' "$work/out" | grep -v '^FAIL triage ids in the registry with no row' || true)
if [[ -n $other ]]; then
  {
    echo
    echo "Other rows the test refused (fix each in a proven-cicd PR, then re-run here):"
    echo '```'
    echo "$other"
    echo '```'
  } >>"$work/cure"
fi
cat "$work/cure"
cat "$work/cure" >>"$summary"
while IFS= read -r id; do
  [[ -n $id ]] && echo "::error title=No row in proven-cicd sq-map.tsv::$id has no row in proven/registry/sq-map.tsv; add it pending (praxis absent) in proven-cicd-and-workflows first, then re-run"
done <<<"$missing"
exit 1
