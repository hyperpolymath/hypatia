#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
#
# proof-ledger.sh — the machine proof ledger (verification/proof-ledger.jsonl)
# is canonical; the Proof Ledger table in verification/PROOF-STATUS.adoc is
# rendered from it (README-EXPLAINME-STANDARD section 5, rule 1).
#
#   proof-ledger.sh check          validate the ledger against its rules and the tree
#   proof-ledger.sh claims         every census README, AFFIRMATION or EXPLAINME quotes is the ledger's
#   proof-ledger.sh render         print the AsciiDoc block for PROOF-STATUS.adoc
#   proof-ledger.sh render --check fail if PROOF-STATUS.adoc holds a different block
#   proof-ledger.sh render --write replace the block in PROOF-STATUS.adoc
#
# One JSON object per line, I-JSON serialised as JCS (RFC 8785): keys sorted,
# no insignificant whitespace, one final LF. The ledger is held to a profile
# in which `jq -cS` output IS the JCS form: ASCII strings and no numbers.
#
# Status vocabulary (closed; PROOF-POLICY section 5 gives the upper-case
# labels): proven, proven-with-postulates, argued, tested-only, open.
# Proof Witness Grades (PROOF-POLICY section 8) are recorded for theory rows
# only: 3, 2i, 2ii, 1 or ungraded. Grades are derived from receipts; this
# check refuses a grade the row's own evidence cannot support.
#
# Exit: 0 = ledger valid (and the block current, for render --check);
#       1 = invalid or stale; 2 = usage or IO error.

set -euo pipefail

LEDGER="${PROOF_LEDGER:-verification/proof-ledger.jsonl}"
DOC="${PROOF_STATUS_DOC:-verification/PROOF-STATUS.adoc}"
BEGIN_MARK='// BEGIN GENERATED: scripts/proof-ledger.sh render (do not edit by hand)'
END_MARK='// END GENERATED: proof ledger'
AGDA_PROFILE='--safe --without-K --cubical-compatible'

[ -f "$LEDGER" ] || { echo "::error::proof-ledger: $LEDGER not found"; exit 2; }

check() {
  local fails=0 n=0 line id unparsed=0
  # Byte profile: every line is its own `jq -cS` rendering, ASCII only, and
  # the file ends in exactly one LF.
  if LC_ALL=C grep -nP '[^\x00-\x7F]' "$LEDGER" >/dev/null; then
    echo "::error::proof-ledger: non-ASCII byte in $LEDGER (the profile is ASCII so that jq -cS is JCS)"; fails=$((fails + 1))
  fi
  if [ -n "$(tail -c1 "$LEDGER")" ] || [ -z "$(tail -c2 "$LEDGER" | head -c1)" ]; then
    echo "::error::proof-ledger: $LEDGER must end in exactly one LF"; fails=$((fails + 1))
  fi
  while IFS= read -r line; do
    n=$((n + 1))
    if ! canon=$(jq -cS . <<<"$line" 2>/dev/null); then
      echo "::error::proof-ledger: line $n is not JSON"; fails=$((fails + 1)); unparsed=1
    elif [ "$canon" != "$line" ]; then
      echo "::error::proof-ledger: line $n is not in canonical (jq -cS / JCS) form"; fails=$((fails + 1))
    fi
  done < "$LEDGER"
  [ "$n" -gt 0 ] || { echo "::error::proof-ledger: $LEDGER has no rows"; exit 1; }
  # The schema pass reads the file as JSON; a line that is not JSON would
  # abort it, so stop here with the lines named.
  if [ "$unparsed" -eq 1 ]; then
    echo "proof-ledger: INVALID ($fails problem(s)) in $LEDGER"
    return 1
  fi

  # Schema and status rules, one message per broken rule.
  local errs
  errs=$(jq -r --arg agda "$AGDA_PROFILE" '
    def need(k; t): if (has(k) and (.[k] | type) == t) then empty else "\(.id // "?"): field \(k) must be a \(t)" end;
    # Field types first: a row with a missing or mistyped field gets only
    # those messages, since the rules below assume the types.
    . as $r
    | [ need("id"; "string"), need("kind"; "string"), need("obligation"; "string"),
        need("status"; "string"), need("checker"; "string"), need("file"; "string"),
        need("names"; "array"), need("axioms"; "array"), need("bounds"; "string"),
        need("trust_boundary"; "string"),
        (if any(.names[]?, .axioms[]?; type != "string") then "\(.id // "?"): names and axioms hold strings" else empty end) ] as $te
    | if ($te | length) > 0 then $te[] else
      ( [keys[] | select(IN("id","kind","obligation","status","checker","file","names","axioms","bounds","trust_boundary","pwg","foundation_diverse","legs") | not)]
        | if length > 0 then "\($r.id): unknown field(s) \(join(", "))" else empty end ),
      ( if (.id | test("^[a-z0-9]+(-[a-z0-9]+)*$")) then empty else "\(.id): id must be lower-case words joined by -" end ),
      ( if IN(.kind; "engineering", "theory") then empty else "\(.id): kind must be engineering or theory" end ),
      ( if IN(.status; "proven", "proven-with-postulates", "argued", "tested-only", "open") then empty
        else "\(.id): status \(.status) is outside the closed vocabulary" end ),
      ( if (.status == "proven" or .status == "proven-with-postulates") and .checker == "-" then "\(.id): a proven row names its checker" else empty end ),
      ( if .status == "proven" and (.axioms | length) > 0 then "\(.id): proven rests on no axioms; with \(.axioms | join(", ")) it is proven-with-postulates" else empty end ),
      ( if .status == "proven-with-postulates" and (.axioms | length) == 0 then "\(.id): proven-with-postulates lists its axioms" else empty end ),
      ( if .status != "proven" and .status != "proven-with-postulates" and (.axioms | length) > 0 then "\(.id): only a proven row carries axioms" else empty end ),
      ( if .status == "tested-only" and .bounds == "" then "\(.id): tested-only gives its bounds" else empty end ),
      ( if .status != "tested-only" and .bounds != "" then "\(.id): only a tested-only row has bounds" else empty end ),
      ( if .status != "open" and .file == "-" then "\(.id): only an open row may have no file" else empty end ),
      ( if .file != "-" and (.names | length) == 0 then "\(.id): a row with a file names what it rests on in that file" else empty end ),
      ( if .kind == "theory" then
          ( if IN(.pwg; "3", "2i", "2ii", "1", "ungraded") then empty else "\(.id): a theory row records pwg as 3, 2i, 2ii, 1 or ungraded" end ),
          ( if (.foundation_diverse | type) == "boolean" then empty else "\(.id): a theory row records foundation_diverse" end ),
          ( if (.legs | type) == "array" then empty else "\(.id): a theory row records its legs" end ),
          ( if .pwg != "ungraded" and ((.status == "proven" or .status == "proven-with-postulates") | not)
            then "\(.id): PWG-\(.pwg) needs an Agda proof that is proven or proven-with-postulates" else empty end ),
          ( if .pwg != "ungraded" and ((.checker | contains("Agda")) and (.checker | contains($agda)) | not)
            then "\(.id): PWG-\(.pwg) needs the PROOF-POLICY 2.1 invocation (\($agda)) in its checker" else empty end ),
          ( if IN(.pwg; "2i", "2ii", "1") and ([.legs[]? | select(.counts == true)] | length) == 0
            then "\(.id): PWG-\(.pwg) needs a counted recheck leg" else empty end )
        else
          ( if has("pwg") or has("legs") or has("foundation_diverse") then "\(.id): pwg, legs and foundation_diverse belong to theory rows" else empty end )
        end )
      end
  ' "$LEDGER")
  if [ -n "$errs" ]; then
    while IFS= read -r e; do echo "::error::proof-ledger: $e"; done <<<"$errs"
    fails=$((fails + $(wc -l <<<"$errs")))
  fi
  local dups
  dups=$(jq -r '.id' "$LEDGER" | sort | uniq -d)
  if [ -n "$dups" ]; then echo "::error::proof-ledger: duplicate id(s): $dups"; fails=$((fails + 1)); fi

  # Currency against the tree: the file exists, and every name it rests on
  # is in it (a renamed proof turns this red).
  local file name
  while IFS=$'\t' read -r id file name; do
    if [ ! -f "$file" ]; then
      echo "::error::proof-ledger: $id names $file, which does not exist"; fails=$((fails + 1)); continue
    fi
    if [ -n "$name" ] && ! grep -qwF -- "$name" "$file"; then
      echo "::error::proof-ledger: $id says $file holds '$name', and it does not (renamed or removed?)"; fails=$((fails + 1))
    fi
  done < <(jq -r 'select(.file != "-" and (.names | type) == "array" and (.file | type) == "string") | .id as $i | .file as $f | if (.names | length) == 0 then [$i, $f, ""] else (.names[] | [$i, $f, .]) end | @tsv' "$LEDGER")

  if [ "$fails" -gt 0 ]; then
    echo "proof-ledger: INVALID ($fails problem(s)) in $LEDGER"
    return 1
  fi
  echo "proof-ledger: $n rows valid; $(census)"
}

# A census, never a percentage (PROOF-POLICY 8.4, README-EXPLAINME 5 rule 2).
census() {
  jq -rs '
    def c(s): [.[] | select(.status == s)] | length;
    def g(p): [.[] | select(.kind == "theory" and .pwg == p)] | length;
    "\(c("proven")) proven of \(length); \(c("proven-with-postulates")) proven-with-postulates, \(c("tested-only")) tested-only, \(c("argued")) argued, \(c("open")) open. Theory rows: \([.[] | select(.kind == "theory")] | length) (PWG-1 \(g("1")), PWG-2ii \(g("2ii")), PWG-2i \(g("2i")), PWG-3 \(g("3")), ungraded \(g("ungraded")))."
  ' "$LEDGER"
}

render() {
  printf '%s\n' "$BEGIN_MARK"
  printf '%s\n' '[[proof-ledger]]'
  printf '%s\n' '=== Proof Ledger'
  printf '\n'
  # shellcheck disable=SC2016  # the backticks are AsciiDoc, printed literally
  printf 'Rendered from `+%s+`, which is canonical: where this table and the\n' "$LEDGER"
  printf 'ledger disagree, the ledger wins and this table is regenerated.\n'
  printf '\n'
  printf '*Census:* %s\n' "$(census)"
  printf '\n'
  printf '%s\n' '[cols="3,1,2,2,2",options="header"]'
  printf '%s\n' '|==='
  printf '%s\n' '|Obligation |Status |Checker (version, flags) |Where |Trust boundary'
  jq -r '
    def esc: gsub("\\|"; "{vbar}");
    def code: "`+" + . + "+`";
    "\n|" + (.obligation | esc)
    + "\n|" + .status
      + (if (.axioms | length) > 0 then " (" + (.axioms | join(", ")) + ")" else "" end)
      + (if .kind == "theory" then (if .pwg == "ungraded" then ", ungraded" else ", PWG-" + .pwg end)
           + (if .foundation_diverse then " +foundation-diverse" else "" end) else "" end)
    + "\n|" + (if .checker == "-" then "-" else (.checker | esc) end)
    + "\n|" + (if .file == "-" then "-" else (.file | code) + (if (.names | length) > 0 then ": " + (.names | map(code) | join(", ")) else "" end) end)
    + "\n|" + (.trust_boundary | esc)
      + (if .bounds != "" then ". Bounds: " + (.bounds | esc) else "" end)
  ' "$LEDGER"
  printf '%s\n' '|==='
  printf '%s\n' "$END_MARK"
}

# Every census a claim document quotes ("N proven of M; ...") must be the
# ledger's census: README's proof count is derived from the ledger, never
# typed in (README-EXPLAINME section 5, rule 2). Lines are joined first, so a
# census wrapped across lines is still read whole.
CLAIM_DOCS="${PROOF_CLAIM_DOCS:-README.adoc AFFIRMATION.adoc EXPLAINME.adoc}"
claims() {
  local want doc found fails=0 seen=0 c
  want=$(census | sed 's/ Theory rows:.*//')
  for doc in $CLAIM_DOCS; do
    [ -f "$doc" ] || continue
    found=$(tr '\n' ' ' < "$doc" | tr -s ' ' | grep -oE '[0-9]+ proven of [0-9]+(; [0-9]+ proven-with-postulates, [0-9]+ tested-only, [0-9]+ argued, [0-9]+ open\.?)?' || true)
    while IFS= read -r c; do
      [ -n "$c" ] || continue
      seen=$((seen + 1))
      [ "${c%.}." = "$want" ] && continue
      echo "::error::proof-ledger: $doc quotes the census '$c'; the ledger's is '$want'"
      fails=$((fails + 1))
    done <<<"$found"
  done
  if [ "$fails" -gt 0 ]; then return 1; fi
  echo "proof-ledger: $seen quoted census(es) agree with the ledger"
}

doc_block() {
  awk -v b="$BEGIN_MARK" -v e="$END_MARK" '$0 == b { on = 1 } on { print } $0 == e { exit }' "$DOC"
}

case "${1:-}" in
  check) check ;;
  claims) claims ;;
  render)
    case "${2:-}" in
      "") render ;;
      --check)
        [ -f "$DOC" ] || { echo "::error::proof-ledger: $DOC not found"; exit 2; }
        if ! grep -qxF -- "$BEGIN_MARK" "$DOC" || ! grep -qxF -- "$END_MARK" "$DOC"; then
          echo "::error::proof-ledger: $DOC lacks the generated-block markers; restore them (render --write needs them too)"; exit 2
        fi
        if ! diff -u <(doc_block) <(render); then
          echo "::error::proof-ledger: the Proof Ledger block in $DOC is not the rendering of $LEDGER; run scripts/proof-ledger.sh render --write"
          exit 1
        fi
        echo "proof-ledger: $DOC block is current" ;;
      --write)
        [ -f "$DOC" ] || { echo "::error::proof-ledger: $DOC not found"; exit 2; }
        if ! grep -qxF -- "$BEGIN_MARK" "$DOC" || ! grep -qxF -- "$END_MARK" "$DOC"; then
          echo "::error::proof-ledger: $DOC lacks the generated-block markers"; exit 2
        fi
        tmp=$(mktemp); blk=$(mktemp)
        render > "$blk"
        # The block is read from a file, not passed with -v: awk would
        # process backslash escapes in a -v value.
        awk -v b="$BEGIN_MARK" -v e="$END_MARK" '
          NR == FNR { block = block $0 "\n"; next }
          $0 == b { printf "%s", block; skip = 1; next }
          $0 == e { skip = 0; next }
          !skip { print }' "$blk" "$DOC" > "$tmp"
        cat "$tmp" > "$DOC"; rm -f "$tmp" "$blk"
        echo "proof-ledger: wrote the block into $DOC" ;;
      *) echo "usage: $0 render [--check|--write]" >&2; exit 2 ;;
    esac ;;
  *) echo "usage: $0 check | claims | render [--check|--write]" >&2; exit 2 ;;
esac
