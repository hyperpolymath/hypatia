#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# check-proof-status.sh — keep verification/PROOF-STATUS.adoc honest (#816).
#
# A proof-status document that is wrong is worse than one that is missing: it
# is read as evidence. This script makes the document go stale LOUDLY:
#
#   1. every source path the document names must exist in the tree
#      (bare filenames are resolved against the proof roots);
#   2. every `+identifier+` named in an inventory row must appear inside
#      that row's file;
#   3. every `+identifier+` named in a "Properties Proven" section must
#      appear inside that section's file.
#
# Rows marked `[line-through]` / `RETIRED` are historical record and are
# skipped. The dangerous-pattern audit names escape hatch SPELLINGS on
# purpose (their count is the claim), so its section is not treated as a
# proof inventory.
#
# Both directions are load-bearing: a renamed proof turns this check red
# (the document claims something the source no longer contains), and a
# document naming a file or proof that never existed is also red. Proven by
# mutant both ways against this tree (rename `connectorCount` in
# src/Hypatia/ABI/Types.idr -> red; revert -> green).
#
# Usage: scripts/check-proof-status.sh [path/to/PROOF-STATUS.adoc]
# Exit:  0 = document agrees with the tree; 1 = stale; 2 = usage/IO error.

set -uo pipefail

DOC="${1:-verification/PROOF-STATUS.adoc}"
ROOTS="src/Hypatia/ABI src/abi verify/src verification/proofs"

if [ ! -f "$DOC" ]; then
  echo "::error::check-proof-status: $DOC not found"
  exit 2
fi

resolve_file() {
  # $1 = path or bare filename. Prints the resolved path or nothing.
  case "$1" in
    */*) [ -f "$1" ] && printf '%s\n' "$1" ;;
    *) find $ROOTS -name "$1" -type f 2>/dev/null | head -1 ;;
  esac
}

# ---------------------------------------------------------------------------
# Extraction. One awk pass emits records for the bash loop below:
#   FILE   <file>                 — a source path named by the document
#   IDENT  <file> <ident>         — an identifier claimed in that file
# Rows/sections marked [line-through] or RETIRED are skipped; the Properties
# Proven section bodies are attached to the file named in their ==== header.
# ---------------------------------------------------------------------------
extract() {
  awk '
    function emit_row(b,   spans, n, i, span, file, first) {
      if (b ~ /line-through/ || b ~ /RETIRED/) return
      n = split(b, spans, /`\+/)
      file = ""
      for (i = 2; i <= n; i += 2) {
        span = spans[i]
        sub(/\+`.*/, "", span)   # cut at the closing +`
        first = span
        sub(/[ \t(:<>=].*/, "", first)
        sub(/^[^A-Za-z_]*/, "", first)
        sub(/[^A-Za-z0-9_'"'"'.]*$/, "", first)
        if (span ~ /\.(idr|lean|tla|agda)/) {
          if (file == "") {
            file = span
            sub(/:.*$/, "", file)
            print "FILE\t" file "\t"
          }
          continue
        }
        if (file != "" && first ~ /^[A-Za-z_][A-Za-z0-9_'"'"'.]*$/)
          print "IDENT\t" file "\t" first
      }
    }
    function emit_section(s,   header, body, spans, n, i, span, file, first) {
      header = s
      sub(/\n.*/, "", header)
      if (header !~ /`\+[^`]+\+`/) return
      file = header
      sub(/^[^`]*`\+/, "", file)
      sub(/\+`.*$/, "", file)
      print "FILE\t" file "\t"
      body = s
      sub(/^[^\n]*\n/, "", body)
      n = split(body, spans, /`\+/)
      for (i = 2; i <= n; i += 2) {
        span = spans[i]
        sub(/\+`.*/, "", span)   # cut at the closing +`
        first = span
        sub(/[ \t(:<>=].*/, "", first)
        sub(/^[^A-Za-z_]*/, "", first)
        sub(/[^A-Za-z0-9_'"'"'.]*$/, "", first)
        if (first ~ /^[A-Za-z_][A-Za-z0-9_'"'"'.]*$/)
          print "IDENT\t" file "\t" first
      }
    }

    # Inventory tables: everything before "Properties Proven".
    /^=== Properties Proven/ {
      in_props = 1
      if (block != "") emit_row(block)
      block = ""
      next
    }
    # Properties Proven sections run until the dangerous-pattern audit.
    /^=== Dangerous Pattern Audit/ {
      if (section != "") emit_section(section)
      section = ""
      in_props = 0
      done = 1
      next
    }
    done { next }

    in_props {
      if (/^==== /) {
        if (section != "") emit_section(section)
        section = $0
        next
      }
      if (section != "") section = section "\n" $0
      next
    }

    # Inventory row accumulation: a row starts at a "|"-line and continues
    # to the next one.
    /^\|/ {
      if (block != "") emit_row(block)
      block = $0
      next
    }
    { if (block != "") block = block "\n" $0 }
    END {
      if (!done && block != "") emit_row(block)
      if (section != "") emit_section(section)
    }
  ' "$DOC"
}

FILES_CHECKED=0
IDENTS_CHECKED=0
FAILURES=0

while IFS=$'\t' read -r kind file ident; do
  case "$kind" in
    FILE)
      FILES_CHECKED=$((FILES_CHECKED + 1))
      if [ -z "$(resolve_file "$file")" ]; then
        echo "::error::PROOF-STATUS names '$file' but no such file exists in the tree (#816)"
        FAILURES=$((FAILURES + 1))
      fi
      ;;
    IDENT)
      resolved="$(resolve_file "$file")"
      [ -z "$resolved" ] && continue # its FILE record reports the miss
      IDENTS_CHECKED=$((IDENTS_CHECKED + 1))
      if ! grep -qwF -- "$ident" "$resolved" 2>/dev/null; then
        echo "::error::PROOF-STATUS says '$ident' is proven in $file, but $resolved does not contain it (renamed or removed? #816)"
        FAILURES=$((FAILURES + 1))
      fi
      ;;
  esac
done < <(extract)

echo "PROOF-STATUS currency: files named ${FILES_CHECKED}, identifiers checked ${IDENTS_CHECKED}, mismatches ${FAILURES}"

if [ "$FAILURES" -gt 0 ]; then
  echo "PROOF-STATUS currency check: STALE — the document disagrees with the tree"
  exit 1
fi

if [ "$FILES_CHECKED" -eq 0 ]; then
  echo "::error::check-proof-status: parsed 0 files from $DOC — the extractor matched nothing, which is not a pass"
  exit 1
fi

echo "PROOF-STATUS currency check: document agrees with the tree"
exit 0
