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
# A second pass (section 6) adds the plain `type` names that findings carry:
# SARIF ruleIds, .hypatia-baseline.json and .hypatia-ignore key on those, not on
# the coded ids. See the comment at section 6 for each vocabulary's source.
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
near() { # <file> <id> [hit-regex for an atom-named rule]
  awk -v id="$2" -v hitre="${3:-}" '
    function comp(l,   t, w) {
      t = l
      while (match(t, /:(critical|high|medium|warn|low|info)/)) {
        w = substr(t, RSTART + 1, RLENGTH - 1); t = substr(t, RSTART + RLENGTH)
        if (rank(w) > rank(best)) best = w
      }
    }
    function rank(w) { return (w == "critical") ? 5 : (w == "high") ? 4 : (w == "medium" || w == "warn") ? 3 : (w == "low") ? 2 : (w == "info") ? 1 : 0 }
    { line[NR] = $0 }
    # A one-line severity helper: defp re001_severity(job), do: if(..., do: :high, else: :warn)
    match($0, /defp [a-z0-9_]*severity[a-z0-9_]*\(/) { nm = substr($0, RSTART + 5, RLENGTH - 6); helper[nm] = $0 }
    hitre != "" ? ($0 ~ hitre) : ($0 ~ ("(rule|id): \"" id "\"") || $0 ~ ("(rule|id): \"" id "[^0-9]")) { hits[++n] = NR }
    END {
      if (!n) { printf "\037\037\n"; exit }
      # Every emitting literal counts. Per literal the map is read 3 lines back
      # and up to 12 forward, never past the next rule literal. The severity is
      # the highest one the rule fires at; the class is advisory only when
      # every literal is advisory (`advisory: true` or `severity: :advisory`).
      best = ""; advn = 0
      for (h = 1; h <= n; h++) {
        isadv = 0; lit = ""; act = ""
        # A severity computed in code from literals, one line:
        #   severity = if projected > 10, do: :high, else: :warn
        #   severity: if(count > 3, do: :critical, else: severity)
        # takes the highest atom on the line. Up to 10 lines back, not past
        # the previous rule literal. A severity read from run-time data
        # (the alert level itself) has no atom and stays empty.
        for (j = hits[h] - 1; j >= hits[h] - 10 && j >= 1; j--) {
          if (line[j] ~ /(rule|id): "[A-Z]/) break
          if (line[j] ~ /severity[: =].*[( ]if[( ]/) comp(line[j])
        }
        for (i = hits[h] - 3; i <= hits[h] + 30; i++) {
          if (i < 1 || !(i in line)) continue
          if (i > hits[h] && line[i] ~ /^ *(rule|id): "[A-Z]/ && line[i] !~ ("\"" id "\"")) break
          # AuditPrevention derives severity from the record action
          # (action_severity/1 in lib/rules/audit_prevention.ex, guarded below).
          if (i > hits[h] && match(line[i], /^ *action: "[a-z_]+"/)) { act = substr(line[i], RSTART, RLENGTH); sub(/^ *action: "/, "", act); sub(/"$/, "", act) }
          if (i > hits[h] + 12) continue
          if (line[i] ~ /severity[: =].*[( ]if[( ]/) comp(line[i])
          if (match(line[i], /severity: [a-z0-9_]+\(/)) { nm = substr(line[i], RSTART + 10, RLENGTH - 11); if (nm in helper) comp(helper[nm]) }
          if (match(line[i], /severity: "?:?(critical|high|medium|warn|low|info)/)) {
            sv = substr(line[i], RSTART, RLENGTH); sub(/.*[:"]/, "", sv)
            lit = sv
            if (rank(sv) > rank(best)) best = sv
          }
          if (line[i] ~ /advisory: true/ || line[i] ~ /severity: :advisory/) isadv = 1
          if (desc == "" && match(line[i], /description: "[^"]+"/)) desc = substr(line[i], RSTART + 14, RLENGTH - 15)
        }
        if (lit == "" && act != "") {
          sv = (act == "block_merge_with_escalation") ? "critical" : (act == "block_commit" || act == "block_merge" || act == "block_release") ? "high" : (act == "warn_and_log") ? "low" : "medium"
          if (rank(sv) > rank(best)) best = sv
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
  } | sed -n 1p | first_sentence | clean
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
      # The row says so in words (PI005: "(stale-pin, advisory)"): advisory.
      cls=gate; d="class excusable-at scope-form"
      case "$text" in *[Aa]dvisory*) cls=advisory; d="excusable-at scope-form" ;; esac
      emit "hypatia/$m/$id" "$(printf '%s' "$text" | clean)" "$(sev_map "$sev")" "$cls" active "" "" "$d"
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
    # The module doc can state that its info findings are advisory and not
    # blocking (DisambiguationRules: `severity: :info` (advisory, not blocking)).
    [ -z "$adv" ] && [ "$sev" = info ] && grep -qE 'severity: :info.*\(advisory' "$f" && adv=advisory
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

# The action to severity mapping near() copies (AuditPrevention): refuse to
# generate when the code no longer has it, so the copy cannot go stale quietly.
for l in 'defp action_severity("block_merge_with_escalation"), do: :critical' \
  'when action in ["block_commit", "block_merge", "block_release"],' \
  'defp action_severity("warn_and_log"), do: :low' 'defp action_severity(_action), do: :medium'; do
  grep -qF -- "$l" lib/rules/audit_prevention.ex ||
    { echo "gen-rules-praxis: lib/rules/audit_prevention.ex no longer has '$l'; update near()" >&2; exit 1; }
done

# Named atom-typed ids: refuse to register one the code no longer has.
while IFS=$'\t' read -r id f lit text; do
  grep -qF -- "$lit" "$f" || { echo "gen-rules-praxis: $f no longer has '$lit' ($id)" >&2; exit 1; }
  # The severity the code states in the 4 lines after the literal, if any
  # (workflow_audit :low, cicd_rules :medium; actions_lock states none).
  n=$(grep -nF -- "$lit" "$f" | sed -n 1p | cut -d: -f1)
  # `|| true`: no severity in the window is a valid answer, not a failure.
  sev=$({ sed -n "${n},$((n + 4))p" "$f" | grep -oE 'severity: :(critical|high|medium|warn|low|info)' || true; } | sed -n 1p | sed 's/.*: ://')
  emit "$id" "$text" "$(sev_map "$sev")" gate active "" "" "class excusable-at scope-form"
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

# 6. The plain names findings carry today (A5 3.3: `rule_module/type` becomes
#    `hypatia/<rule_module>/<type>`). SARIF ruleIds, .hypatia-baseline.json and
#    .hypatia-ignore key on these, so a deviation resolves only if they are here.
#    Source of each vocabulary (what lib/hypatia/cli.ex emits as `type`):
#      code_safety       `id: :name` entries with a `pattern:` in code_safety.ex
#      content_patterns  `id: :name` entries with `applies_to:` in the
#                        @blocked_patterns table of cicd_rules.ex
#      honest_completion, green_web, root_hygiene, workflow_audit: `type: :name`
#                        atoms (workflow_audit also `rule: "name"` strings)
#      scorecard         the `category: "Name"` of each check in scorecard_ingestor.ex
#      fixed names       literals in cli.ex / the rule files, each guarded below
# An id that already has a row (a coded id, a named or superseded one) is skipped.
has_row() { awk -F'\t' -v i="$1" '$1 == i { f = 1 } END { exit !f }' "$rows"; }

# Portability: gawk (the CI runner) and mawk (some dev machines) read a backslash
# in a `-v` value differently: gawk turns `\[` into `[`, an invalid regex, and the
# awk dies inside a process substitution with no error here. Pass no backslash in
# a `-v` value; write a bracket as `[[]`.
# Entries of a pattern table: "name US severity US text". One entry runs from its
# `id: :name,` line to the next one; `need` names a key it must contain.
entries() { # <file> <need-regex> [block-start-regex]
  awk -v need="$2" -v bstart="${3:-}" '
    function flush() { if (name != "" && (need == "" || has)) printf "%s\037%s\037%s\n", name, sev, txt; name = ""; sev = ""; txt = ""; has = 0; pend = 0 }
    bstart != "" && $0 ~ bstart { inb = 1 }
    bstart != "" && inb && /^  \]/ { flush(); inb = 0 }
    bstart != "" && !inb { next }
    match($0, /^ *id: :[a-z0-9_]+,/) { flush(); name = $0; sub(/^ *id: :/, "", name); sub(/,.*/, "", name); next }
    name == "" { next }
    need != "" && $0 ~ need { has = 1 }
    sev == "" && match($0, /severity: :(critical|high|medium|warn|low|info)/) { sev = substr($0, RSTART + 11, RLENGTH - 11) }
    txt == "" && pend && match($0, /"[^"]+"/) { txt = substr($0, RSTART + 1, RLENGTH - 2); pend = 0 }
    txt == "" && match($0, /(description|reason): *$/) { pend = 1 }
    txt == "" && match($0, /(description|reason): *"[^"]+"/) { t = substr($0, RSTART, RLENGTH); sub(/^[a-z]+: *"/, "", t); sub(/"$/, "", t); txt = t }
    END { flush() }' "$1"
}

# emit one emitted-name row; $4 is the text ("" gives the documented placeholder).
emit_name() { # <module> <name> <severity-word> <text> [advisory]
  local id="hypatia/$1/$2" d="class excusable-at scope-form" text cls=gate
  has_row "$id" && return 0
  text=$(printf '%s' "$4" | first_sentence | clean)
  [ -z "$text" ] && { text="$id in lib/rules/$1.ex."; d="text $d"; }
  [ -n "${5:-}" ] && { cls=advisory; d="${d/class /}"; }
  emit "$id" "$text" "$(sev_map "$3")" "$cls" active "" "" "$d"
}

while IFS=$'\037' read -r name sev txt; do
  emit_name code_safety "$name" "$sev" "$txt"
done < <(entries lib/rules/code_safety.ex 'pattern:')
# cli.ex gives a content-pattern finding `Map.get(f, :severity, "medium")`, so an
# entry that states no severity is medium.
grep -qF 'severity: to_string(Map.get(f, :severity, "medium"))' lib/hypatia/cli.ex ||
  { echo "gen-rules-praxis: cli.ex no longer defaults content_patterns severity to medium; update the generator" >&2; exit 1; }
while IFS=$'\037' read -r name sev txt; do
  emit_name content_patterns "$name" "${sev:-medium}" "$txt"
done < <(entries lib/rules/cicd_rules.ex 'applies_to:' '^  @blocked_patterns [[]')

# A finding that declares `rule_id: "hypatia/<other>/<name>"` is emitted under
# that module and name (cli.ex splits it), so the same name under the source
# module is never emitted: WF029 `always_after_cancel` is ci_failure_triage's.
moved() { grep -qE "rule_id: \"hypatia/[a-z_]+/$2\"" "$1" && ! grep -qE "rule_id: \"hypatia/$3/$2\"" "$1"; }

# $1 module, $2 names (one per line), $3 hit regex builder: names read from `type:`
# atoms or from lowercase `rule: "name"` strings.
emit_found() { # <module> <names...>
  local m=$1 f=lib/rules/$1.ex name sev adv desc; shift
  for name in $(printf '%s\n' "$@" | sort -u); do
    [ -z "$name" ] && continue
    moved "$f" "$name" "$m" && continue
    IFS=$'\037' read -r sev adv desc <<<"$(near "$f" "$name" "(type|rule): (:|\")$name([^a-z0-9_]|\$)")"
    emit_name "$m" "$name" "$sev" "$desc" "$adv"
  done
}
atoms() { { grep -ohE '^ *type: :[a-z][a-z0-9_]+' "$1" | sed 's/.*: ://'; } || true; }
strs()  { { grep -ohE '^ *rule: "[a-z][a-z0-9_]+"' "$1" | sed 's/.*: "//; s/"$//'; } || true; }
for m in honest_completion green_web root_hygiene; do
  emit_found "$m" "$(atoms lib/rules/$m.ex)"
done
emit_found workflow_audit "$(atoms lib/rules/workflow_audit.ex)" "$(strs lib/rules/workflow_audit.ex)"
# workflow_hardening emits `f.rule`: the WH codes (above) and one lowercase name.
emit_found workflow_hardening "$(strs lib/rules/workflow_hardening.ex)"

# Scorecard: one type per check category.
for name in $({ grep -ohE 'category: "[A-Za-z]+"' lib/scorecard_ingestor.ex | sed 's/.*: "//; s/"$//'; } | sort -u); do
  emit_name scorecard "$name" "" "OpenSSF Scorecard check category $name."
done

# Fixed names: the literal that emits each, and the severity (and advisory flag)
# the code states, each proved too. A severity proof in the same file must sit
# within 3 lines of the emitting literal; one in another file must appear there.
# `~` separates the columns: read collapses empty tab-separated fields.
while IFS='~' read -r mod name f lit sev adv sf slit text; do
  grep -qF -- "$lit" "$f" || { echo "gen-rules-praxis: $f no longer has '$lit' ($mod/$name)" >&2; exit 1; }
  if [ "$sf" = "$f" ]; then  # same file: beside the literal (a trailing #any: anywhere in it)
    n=$(grep -nF -- "$lit" "$f" | sed -n 1p | cut -d: -f1)
    sed -n "$((n - 3)),$((n + 3))p" "$f" | grep -qF -- "$slit" ||
      { echo "gen-rules-praxis: $f no longer states '$slit' beside '$lit' ($mod/$name)" >&2; exit 1; }
  else
    sf=${sf%#any}
    grep -qF -- "$slit" "$sf" ||
      { echo "gen-rules-praxis: $sf no longer has '$slit' ($mod/$name)" >&2; exit 1; }
  fi
  emit_name "$mod" "$name" "$sev" "$text" "$adv"
done <<'EOF'
cicd_rules~missing_requirement~lib/hypatia/cli.ex~type: "missing_requirement"~high~~lib/hypatia/cli.ex~severity: "high",~A file the repo must have is missing.
cicd_rules~banned_language_file~lib/hypatia/cli.ex~type: "banned_language_file"~critical~~lib/rules/code_safety.ex~severity: :critical,~A file in a banned language is in the repo.
workflow_audit~no_workflows_dir~lib/hypatia/cli.ex~type: "no_workflows_dir"~high~~lib/hypatia/cli.ex~severity: "high",~The repo has no .github/workflows directory.
honest_completion~inflated_completion~lib/hypatia/cli.ex~type: "inflated_completion"~high~~lib/hypatia/cli.ex~severity: "high",~The claimed completion is far above the evidence.
migration_rules~deprecated_api~lib/hypatia/cli.ex~type: "deprecated_api"~high~~lib/rules/migration_rules.ex~severity: :high,~A deprecated API is in use.
security_errors~secret_detected~lib/hypatia/cli.ex~rule_type = "secret_detected"~critical~~lib/hypatia/scanner_suppression.ex~{"critical", "revoke_rotate_and_purge"~A line holds what looks like a literal secret.
structured_format~SF001~lib/rules/structured_format.ex~type: "SF001"~medium~advisory~lib/rules/structured_format.ex~severity: "medium",~A regex rebuilds a workflow field that a structured reader should read.
implementation_inside_canon~HYP-S009~lib/rules/implementation_inside_canon.ex~@rule_id "HYP-S009"~medium~~lib/rules/implementation_inside_canon.ex#any~rule.severity || :medium~An implementation manifest sits inside a local canonical spec home.
EOF

# A5 RL3/R08: no two rows share a text. Where the code gives one text to two
# modules (CSA002/SSA002 "Severity summary"), qualify each with its module.
awk -F'\t' -v OFS='\t' 'NR == FNR { c[$2]++; next } c[$2] > 1 { split($1, p, "/"); $2 = $2 " (" p[2] ")" } { print }' "$rows" "$rows" >"$rows.t"
mv "$rows.t" "$rows"

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
  # Owner ruling Q2 (2026-11-14): an identity field takes v8 only, never #u5.
  # The same estate chora id as hypatia_chora.deed (hypatia#1095).
  echo '  :beholding-chora #uuid"0889b37b-defb-831f-973c-e119500db00e"'
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
