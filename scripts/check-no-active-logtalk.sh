#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# check-no-active-logtalk.sh — keep Logtalk retired (ADR-001, 2026-04-14).
#
# Hypatia's canonical rule system is pure Elixir (lib/rules/*.ex); there
# is no Logtalk loader, rule format, or report format. This guard fails
# if an *active* Logtalk form re-enters the tracked tree:
#
#   1. a *.lgt / *.logtalk source file;
#   2. an embedded Logtalk source block or object directive, a
#      logtalk_load/tester/doclet invocation;
#   3. a configuration setting requesting Logtalk (report-format /
#      format / --format, A2ML or DEED spelling, loader/output paths
#      ending in .lgt);
#   4. in executable/config files, a non-comment .lgt reference or a
#      generated security_rule(...) fact.
#
# Historical *prose* about Logtalk is not banned; the ledger, dated
# audits, and provenance notes are records, not settings. Files that
# must quote an active-looking form to document its retirement are
# exempted by exact path in .logtalk-allowlist (shrink-only, each with
# a classification and removal condition). Comment-only lines are not
# settings and are ignored.
#
# Usage: scripts/check-no-active-logtalk.sh [--all]
#   default: tracked files (git ls-files)
#   --all:   also untracked, non-ignored files (local pre-commit use)
# Exit: 0 clean; 1 = active Logtalk form or stale/malformed allowlist;
#       2 = usage / IO error.

set -uo pipefail

ROOT="$(git rev-parse --show-toplevel 2>/dev/null)" || {
  echo "::error::check-no-active-logtalk: not inside a git repository" >&2
  exit 2
}
cd "$ROOT" || exit 2

ALLOWLIST=".logtalk-allowlist"
SELF="scripts/check-no-active-logtalk.sh"

mode="tracked"
case "${1:-}" in
  "") ;;
  --all) mode="all" ;;
  *) echo "usage: $0 [--all]" >&2; exit 2 ;;
esac
if (( $# > 1 )); then
  echo "usage: $0 [--all]" >&2
  exit 2
fi

[[ -f $ALLOWLIST ]] || {
  echo "::error::check-no-active-logtalk: $ALLOWLIST not found" >&2
  exit 2
}

# ---------------------------------------------------------------------------
# Guarded forms.
# ---------------------------------------------------------------------------
active_re='\[source,logtalk\]|:-[[:space:]]*(object|end_object|protocol|category)[[:space:]]*\(|logtalk_load[[:space:]]*\(|logtalk_(tester|doclet)|report-format[[:space:]]*=[[:space:]]*["'"'"']?logtalk|:report-format[[:space:]]+"logtalk"|(report[-_]?format|format)[[:space:]]*:[[:space:]]*["'"'"']?logtalk|--format([[:space:]]+|=)logtalk|^[[:space:]]*(loader|output_path|output|path|file|rules?)[[:space:]]*[:=].*\.lgt'
codegen_re='\.lgt(["'"'"'[:space:],)\]]|$)|security_rule[[:space:]]*\('

is_comment_line() {
  local input_line="$1" trimmed
  trimmed="$(printf '%s' "$input_line" | sed 's/^[[:space:]]*//')"
  case "$trimmed" in
    '#'*) return 0 ;;
    ';'*) return 0 ;;
    '//'*) return 0 ;;
    '--'*) return 0 ;;
    '%'*) return 0 ;;
    *) return 1 ;;
  esac
}

is_codegen_file() {
  local candidate_path="$1"
  case "$candidate_path" in
    *.ex|*.exs|*.sh|*.bash|*.yml|*.yaml|*.toml|*.json|*.ncl|*.a2ml|\
    *.deed|*.contractile|*.rs|*.py|*.js|*.ts|*.jl|*.hs|*.erl|*.gleam|\
    *.zig|*.idr|*.agda) return 0 ;;
    *) return 1 ;;
  esac
}

# has_candidate <file>: raw (comment-inclusive) presence of a guarded
# form — used only to detect allowlist entries that have gone stale.
has_candidate() {
  local candidate_file="$1"
  grep -IqiE -- "$active_re" "$candidate_file" 2>/dev/null && return 0
  if is_codegen_file "$candidate_file"; then
    grep -IqiE -- "$codegen_re" "$candidate_file" 2>/dev/null && return 0
  fi
  return 1
}

# ---------------------------------------------------------------------------
# Parse + validate the allowlist.
# ---------------------------------------------------------------------------
declare -A allowed=()
failures=0
ln=0
while IFS= read -r line || [[ -n $line ]]; do
  ln=$((ln + 1))
  case "$line" in
    ''|'#'*) continue ;;
    *) ;;
  esac
  IFS='|' read -r path class removal extra <<<"$line"
  path="$(printf '%s' "$path" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')"
  class="$(printf '%s' "${class:-}" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')"
  removal="$(printf '%s' "${removal:-}" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')"
  if [[ -n ${extra:-} || -z $path || -z $class || -z $removal ]]; then
    echo "::error::$ALLOWLIST:$ln: expected '<path> | <classification> | <removal condition>'"
    failures=$((failures + 1)); continue
  fi
  case "$path" in
    *'*'*|*'?'*|*'['*|*']'*|'/'*|*'..'*|*'/')
      echo "::error::$ALLOWLIST:$ln: '$path' is not an exact repository-relative file path (no wildcards, no directories)"
      failures=$((failures + 1)); continue ;;
    *) ;;
  esac
  if [[ -n ${allowed[$path]:-} ]]; then
    echo "::error::$ALLOWLIST:$ln: duplicate entry for '$path'"
    failures=$((failures + 1)); continue
  fi
  allowed["$path"]=1
  if [[ ! -f $path ]]; then
    echo "::error::$ALLOWLIST:$ln: allowlisted path no longer exists: $path (shrink the list)"
    failures=$((failures + 1)); continue
  fi
  case "$path" in
    *.lgt|*.logtalk) ;;
    *)
      if ! has_candidate "$path"; then
        echo "::error::$ALLOWLIST:$ln: stale exemption — $path no longer contains a guarded form (shrink the list)"
        failures=$((failures + 1))
      fi ;;
  esac
done <"$ALLOWLIST"

# ---------------------------------------------------------------------------
# Scan.
# ---------------------------------------------------------------------------
if [[ $mode == "all" ]]; then
  mapfile -d '' files < <(git ls-files --cached --others --exclude-standard -z)
else
  mapfile -d '' files < <(git ls-files -z)
fi

checked=0
for f in "${files[@]}"; do
  [[ -n $f ]] || continue
  [[ -f $f ]] || continue
  checked=$((checked + 1))

  lower="$(printf '%s' "$f" | tr '[:upper:]' '[:lower:]')"
  case "$lower" in
    *.lgt|*.logtalk)
      if [[ -z ${allowed[$f]:-} ]]; then
        echo "::error::$f: active Logtalk source file (Hypatia rules are Elixir in lib/rules/*.ex; see docs/rules/logtalk-migration.adoc)"
        failures=$((failures + 1))
      fi
      continue ;;
    *) ;;
  esac

  [[ $f == "$ALLOWLIST" ]] && continue
  [[ -n ${allowed[$f]:-} ]] && continue

  patterns="$active_re"
  if is_codegen_file "$f"; then
    patterns="$active_re|$codegen_re"
  fi

  while IFS= read -r hit; do
    [[ -n $hit ]] || continue
    lineno="${hit%%:*}"
    text="${hit#*:}"
    is_comment_line "$text" && continue
    echo "::error::$f:$lineno: active Logtalk form (retired; canonical replacement is Elixir + JSON — see docs/rules/logtalk-migration.adoc)"
    failures=$((failures + 1))
  done < <(grep -InE -- "$patterns" "$f" 2>/dev/null || true)
done

echo "check-no-active-logtalk: files checked ${checked}, allowlisted ${#allowed[@]}, violations ${failures}"
if (( failures > 0 )); then
  echo "check-no-active-logtalk: FAIL"
  exit 1
fi
echo "check-no-active-logtalk: OK — no active Logtalk forms outside the exact-path allowlist"
exit 0
