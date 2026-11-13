#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# gen-rules-praxis.sh: regenerate hypatia_rules_praxis.deed, the standards A5
# rule registry of the `hypatia` rule domain, from the code.
#
#   scripts/gen-rules-praxis.sh           write hypatia_rules_praxis.deed
#   scripts/gen-rules-praxis.sh --check   exit 1 when the committed file is stale
#   scripts/gen-rules-praxis.sh --stdout  print the registry, write nothing
#
# A5 (metadatastician/standards docs/estate-reconciliation/A5-rules-and-ignore-files.adoc)
# maps an existing id by PREFIX only: `<module>/<ID>` becomes
# `hypatia/<module>/<ID>`. No id that hypatia emits is changed here.
#
# Sources, all read from the tree (same extraction as gen-rule-catalogue.sh):
#   1. lib/rules/*.ex: prefixed ids in `rule:`/`id:` literals, and the rows of
#      a moduledoc table `| ID | severity | text |` (pr_automerge.ex).
#   2. lib/rules/*.ex: ids already in A5 form, a module with
#      `@domain "hypatia/<module>"` and `name: "<name>"` rule maps
#      (ci_failure_triage.ex, hypatia#1064). Taken as they are.
#   3. lib/pattern_registry.ex: the panic-attack categories `"Cat" => "PAnnn"`.
#   4. `# ─── <ID>: TOMBSTONE` comments: the id is kept, `:status retired`.
#   5. The SUPERSEDE table below: the owner ruling of 2026-11-13 on clashing
#      ids. Each newer meaning gets a new id with `:supersedes`; the old id
#      stays `active` (hypatia still emits it) and gets `:superseded-by`.
#
# Fields the code states are taken from it: `:severity` from the severity
# atom of the finding literal (warn -> medium, low/info -> minor) and `:class
# advisory` from `advisory: true`. Every other judgement field is a documented
# default, and the row carries a `;; defaulted:` comment naming it:
#   :class        gate         (A5 4.1 sample; a finding fails the scan)
#   :excusable-at repo         (A5 G3: default for a canon gate rule)
#   :scope-form   exact-paths  (A5 4.1 sample; no glob deviations)
#   :text         "<ID> in lib/rules/<module>.ex." when no one-line text is found
# `auto_fixable` is read by nothing: A5 has no field it maps to.
#
# Output is deterministic (no date, rows sorted by id), so a second run is
# byte-identical (A5 GEN1/GEN2).
set -euo pipefail
cd "$(dirname "$0")/.."
LC_ALL=C
export LC_ALL
out="hypatia_rules_praxis.deed"
mode="${1:-write}"

# old-id <TAB> new-id <TAB> text of the new id (empty: the old id's text).
# The kept meaning is the oldest, by `git log -S` on origin/main.
SUPERSEDE=$(
  cat <<'EOF'
hypatia/pr_automerge/PA001	hypatia/pr_automerge/denylisted_pin
hypatia/pr_automerge/PA002	hypatia/pr_automerge/major_version_jump
hypatia/pr_automerge/PA003	hypatia/pr_automerge/version_disagreement
hypatia/pr_automerge/PA004	hypatia/pr_automerge/unresolved_version_delta
hypatia/pr_automerge/PA005	hypatia/pr_automerge/manifest_without_lockfile
hypatia/pr_automerge/PA006	hypatia/pr_automerge/not_automatable
hypatia/workflow_audit/ERR-WF-013	hypatia/workflow_audit/missing_timeout_minutes	A workflow job declares no timeout-minutes.
hypatia/workflow_audit/WF-025	hypatia/workflow_audit/WF025
hypatia/cicd_rules/duplicate_workflow	hypatia/cicd_rules/waste_duplicate_workflow
hypatia/actions_lock/duplicate_workflow	hypatia/actions_lock/duplicate_workflow_entry
EOF
)

# Atom-typed ids the ruling names, with the literal that proves each in code.
# id <TAB> file <TAB> fixed-string literal <TAB> text
NAMED=$(
  cat <<'EOF'
hypatia/workflow_audit/duplicate_workflow	lib/rules/workflow_audit.ex	type: :duplicate_workflow,	Two or more workflow files are byte-identical.
hypatia/cicd_rules/duplicate_workflow	lib/rules/cicd_rules.ex	id: :duplicate_workflow,	CI/CD waste: multiple workflows do the same thing.
hypatia/actions_lock/duplicate_workflow	lib/rules/actions_lock.ex	{:duplicate_workflow, path}	The actions.lock file names one workflow twice.
EOF
)

idre='[A-Z]{2,3}-?[A-Z]*-?[0-9]{3}'
dash=$'\xe2\x80\x94' # U+2014, written as bytes so this file holds none

# Clean a text for a deed STRING: one line, no dash U+2014/U+2015, escaped.
clean() {
  sed -e 's/\xe2\x80\x94/-/g; s/\xe2\x80\x95/-/g; s/\t/ /g; s/  */ /g; s/^ //; s/ $//' \
    -e 's/\\/\\\\/g; s/"/\\"/g' | cut -c1-240
}

# Keep the first sentence: up to the first ". " that a capital follows.
first_sentence() { sed -E 's/^(([^.]|[.][^ ])*[.])  ?[A-Z].*/\1/'; }

# The severity and class the code states next to the first literal of $id.
# Prints "<severity> US <advisory?> US <description>" (US = \037, not a tab:
# IFS would merge empty tab fields); a field is empty when the code is silent.
near() { # <file> <id>
  awk -v id="$2" '
    function rank(w) { return (w == "critical") ? 5 : (w == "high") ? 4 : (w == "medium" || w == "warn") ? 3 : (w == "low") ? 2 : (w == "info") ? 1 : 0 }
    { line[NR] = $0 }
    $0 ~ ("(rule|id): \"" id "\"") || $0 ~ ("(rule|id): \"" id "[^0-9]") { hits[++n] = NR }
    END {
      if (!n) { printf "\037\037\n"; exit }
      # Every emitting literal counts. Per literal the map is read 3 lines back
      # and up to 12 forward, never past the next rule literal. The severity is
      # the highest one the rule fires at; the class is advisory only when
      # every literal is advisory (`advisory: true` or `severity: :advisory`).
      best = ""; advn = 0
      for (h = 1; h <= n; h++) {
        isadv = 0
        for (i = hits[h] - 3; i <= hits[h] + 12; i++) {
          if (i < 1 || !(i in line)) continue
          if (i > hits[h] && line[i] ~ /^ *(rule|id): "[A-Z]/ && line[i] !~ ("\"" id "\"")) break
          if (match(line[i], /severity: "?:?(critical|high|medium|warn|low|info)/)) {
            sv = substr(line[i], RSTART, RLENGTH); sub(/.*[:"]/, "", sv)
            if (rank(sv) > rank(best)) best = sv
          }
          if (line[i] ~ /advisory: true/ || line[i] ~ /severity: :advisory/) isadv = 1
          if (desc == "" && match(line[i], /description: "[^"]+"/)) desc = substr(line[i], RSTART + 14, RLENGTH - 15)
        }
        advn += isadv
      }
      printf "%s\037%s\037%s\n", best, (advn == n ? "advisory" : ""), desc
    }' "$1"
}

sev_map() {
  case "$1" in
    critical) echo critical ;; high) echo high ;; medium | warn) echo medium ;;
    low | info) echo minor ;; *) echo "" ;;
  esac
}

# The one-line text the code gives $id: a `─── ID: title ───` or
# `─── title (ID) ───` header, else an `ID: text` or `ID <U+2014> text` doc line.
# Empty when there is none.
text_of() { # <file> <id>
  {
    grep -m1 -E "─── $2( ·|:) " "$1" | sed -E "s/.*─── $2( ·|:) //; s/ *(─)+ *$//" || true
    grep -m1 -E "─── .* \($2\) (─)+" "$1" | sed -E "s/.*─── //; s/ \($2\) (─)+ *$//" || true
    grep -m1 -E "^ *(## |@doc \")?$2(:| $dash) [A-Za-z\`]" "$1" | sed -E "s/^ *(## |@doc \")?$2(:| $dash) //; s/\"$//" || true
  } | head -1 | first_sentence | clean
}

rows=$(mktemp)
trap 'rm -f -- "$rows"' EXIT

# row: id, text, severity, class, status, supersedes, superseded-by, defaulted
emit() { printf '%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n' "$@" >>"$rows"; }

for f in lib/rules/*.ex; do
  m=$(basename "$f" .ex)

  # 1b. moduledoc table rows: | PA001 | critical | text |
  table=$({ grep -E "^ *\| $idre \| (critical|high|medium|low|info|warn) \| " "$f" || true; })
  if [ -n "$table" ]; then
    while IFS='|' read -r _ id sev text _; do
      id=$(echo "$id" | tr -d ' '); sev=$(echo "$sev" | tr -d ' ')
      emit "hypatia/$m/$id" "$(printf '%s' "$text" | clean)" "$(sev_map "$sev")" gate active "" "" "class excusable-at scope-form"
    done <<<"$table"
  fi

  # 1a. prefixed ids in rule:/id: literals (gen-rule-catalogue.sh's regex).
  ids=$({ grep -ohE "(rule|id): \"?$idre\"?" "$f" || true; } | { grep -oE "$idre" || true; } | sort -u)
  for id in $ids; do
    # Skip only an id already emitted from the severity table above; a
    # provenance table (`| BP001 | CIS GH 1.1.x | ... |`) is not one.
    grep -qE "^ *\| $id \| (critical|high|medium|low|info|warn) \| " "$f" && continue
    IFS=$'\037' read -r sev adv desc <<<"$(near "$f" "$id")"
    text=$(text_of "$f" "$id")
    [ -z "$text" ] && text=$(printf '%s' "$desc" | clean)
    d=""
    [ -z "$text" ] && { text="$id in lib/rules/$m.ex."; d="text "; }
    class=${adv:-gate}
    [ -z "$adv" ] && d="${d}class "
    emit "hypatia/$m/$id" "$text" "$(sev_map "$sev")" "$class" active "" "" "${d}excusable-at scope-form"
  done

  # 2. ids already in A5 form: `@domain "hypatia/<module>"` and one
  #    `name: "<name>"` per rule map, the text the first line of its `reason:`.
  if grep -qF "@domain \"hypatia/$m\"" "$f"; then
    awk '
      /^ *name: "[a-z0-9_]+",$/ { if (n != "") print n "\037" t; n = $0; sub(/^ *name: "/, "", n); sub(/",$/, "", n); t = ""; r = 0; next }
      n != "" && r == 0 && /^ *reason:/ { r = 1 }
      n != "" && r == 1 && match($0, /"[^"]+"/) { t = t substr($0, RSTART + 1, RLENGTH - 2); if ($0 !~ /<>$/) r = 2 }
      END { if (n != "") print n "\037" t }' "$f" |
      while IFS=$'\037' read -r name text; do
        text=$(printf '%s' "$text" | first_sentence | clean)
        d=""
        [ -z "$text" ] && { text="hypatia/$m/$name in lib/rules/$m.ex."; d="text "; }
        emit "hypatia/$m/$name" "$text" "" gate active "" "" "${d}class excusable-at scope-form"
      done
  fi

  # 4. tombstones.
  for id in $({ grep -ohE "# ─── $idre: TOMBSTONE" "$f" || true; } | grep -oE "$idre"); do
    emit "hypatia/$m/$id" "Retired: $id was never implemented and its slot is not reused." "" gate retired "" "" "class excusable-at scope-form"
  done
done

# 3. panic-attack categories.
grep -oE '"[A-Za-z]+" => "PA[0-9]{3}"' lib/pattern_registry.ex | while IFS='"' read -r _ cat _ id _; do
  emit "hypatia/pattern_registry/$id" "panic-attack weak-point category $cat." "" gate active "" "" "class excusable-at scope-form"
done

# Named atom-typed ids: refuse to register one the code no longer has.
while IFS=$'\t' read -r id f lit text; do
  grep -qF -- "$lit" "$f" || { echo "gen-rules-praxis: $f no longer has '$lit' ($id)" >&2; exit 1; }
  emit "$id" "$text" "" gate active "" "" "class excusable-at scope-form"
done <<<"$NAMED"

# 5. supersession: point the old row at the new id; add the new row.
while IFS=$'\t' read -r old new newtext; do
  awk -F'\t' -v o="$old" '$1 == o { n++ } END { exit n == 1 ? 0 : 1 }' "$rows" ||
    { echo "gen-rules-praxis: superseded id $old is not exactly one row" >&2; exit 1; }
  awk -F'\t' -v OFS='\t' -v o="$old" -v n="$new" '$1 == o { $7 = n } { print }' "$rows" >"$rows.t"
  mv "$rows.t" "$rows"
  if awk -F'\t' -v n="$new" '$1 == n { f = 1 } END { exit !f }' "$rows"; then
    # The new id already exists (WF025): it only gains :supersedes.
    awk -F'\t' -v OFS='\t' -v o="$old" -v n="$new" '$1 == n { $6 = ($6 == "" ? o : $6 " " o) } { print }' "$rows" >"$rows.t"
    mv "$rows.t" "$rows"
  else
    awk -F'\t' -v OFS='\t' -v o="$old" -v n="$new" -v t="$newtext" \
      '$1 == o { $1 = n; if (t != "") { $2 = t; sub(/^text /, "", $8) }; $5 = "active"; $6 = o; $7 = ""; print }' "$rows" >>"$rows"
  fi
  # The old row keeps its meaning but not the same text (A5 RL3, R08).
  awk -F'\t' -v OFS='\t' -v o="$old" -v n="$new" '$1 == o { $2 = "Superseded by " n " (same rule; this id stays valid and is not changed at its source). " $2 } { print }' "$rows" >"$rows.t"
  mv "$rows.t" "$rows"
done <<<"$SUPERSEDE"

render() {
  echo ';; SPDX-License-Identifier: MPL-2.0'
  echo ';;'
  echo ';; GENERATED by scripts/gen-rules-praxis.sh. Do not edit by hand:'
  echo ';; run the script and commit the result. `scripts/gen-rules-praxis.sh --check` fails'
  echo ';; when this file differs from a fresh generation.'
  echo ';;'
  echo ';; The standards A5 rule registry of the `hypatia` rule domain. Each id is'
  echo ';; an id hypatia already has, prefixed `hypatia/<module>/` (A5 3.3).'
  echo ';; A `;; defaulted:` comment names the fields of a row that the code does'
  echo ';; not state; the defaults are documented in the generator.'
  echo '(praxis-deed'
  echo '  :schema-version "1.0.0"'
  echo '  :canonical-name "hypatia-rules"'
  echo '  :beholding-chora #u5"estate/chora"'
  echo '  (rule-domain :name "hypatia" :owner "hyperpolymath/hypatia")'
  echo '  (rules'
  sort -t$'\t' -k1,1 "$rows" | awk -F'\t' '
    {
      if (n++) print ""
      printf "    ;; defaulted: %s\n", $8
      printf "    (rule-def\n      :id \"%s\"\n      :text \"%s\"\n      :class %s\n", $1, $2, $4
      if ($3 != "") printf "      :severity %s\n", $3
      printf "      :excusable-at repo\n      :scope-form exact-paths\n"
      if ($6 != "") {
        k = split($6, s, " "); printf "      :supersedes ("
        for (i = 1; i <= k; i++) printf "%s\"%s\"", (i > 1 ? " " : ""), s[i]
        printf ")\n"
      }
      if ($7 != "") printf "      :superseded-by \"%s\"\n", $7
      printf "      :status %s)", $5
    }
    END { print ")" }'
  echo ')'
}

case "$mode" in
  write) render >"$out" && echo "wrote $out ($(grep -c '(rule-def' "$out") rules)" ;;
  --stdout) render ;;
  --check)
    if ! render | cmp -s - "$out"; then
      echo "$out is stale: run scripts/gen-rules-praxis.sh and commit the result." >&2
      render | diff -u "$out" - >&2 || true
      exit 1
    fi
    echo "$out matches the code." ;;
  *) echo "usage: $0 [--check|--stdout]" >&2; exit 2 ;;
esac
