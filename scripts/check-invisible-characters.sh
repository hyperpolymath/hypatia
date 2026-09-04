#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Byte-safe scanner for invisible Unicode encodings and forbidden C0 controls.
set -u

scan_root="${1:-}"
results_file="${2:-}"
blocking_results_file="${3:-}"
grep_bin="${INVISIBLE_GREP_BIN:-grep}"
find_bin="${INVISIBLE_FIND_BIN:-find}"

if [[ -z "$scan_root" || ! -d "$scan_root" || -z "$results_file" ]]; then
  echo "usage: $0 SCAN_ROOT RESULTS_FILE" >&2
  exit 2
fi

# Scan bytes under the C locale. This detects UTF-8 encodings even when another
# byte in the file is invalid UTF-8, while excluding permitted TAB/LF/CR bytes.
pattern='[\x00-\x08\x0B\x0C\x0E-\x1F]|\xC2(?:\xA0|\xAD)|\xE2\x80[\x8B-\x8F\xAA-\xAF]|\xE2\x81(?:\xA0|[\xA6-\xA9])|\xEF\xBB\xBF'
blocking_pattern='[\x00-\x08\x0B\x0C\x0E-\x1F]'
: > "$results_file" || exit 2
if [[ -n "$blocking_results_file" ]]; then
  : > "$blocking_results_file" || exit 2
fi
scan_error=0
enumeration_file="$(mktemp /tmp/rsr-invisible-files.XXXXXX)" || exit 2
# Invoked indirectly by the EXIT trap.
# shellcheck disable=SC2329
cleanup() {
  rm -f -- "$enumeration_file"
}
trap cleanup EXIT

if ! "$find_bin" "$scan_root" \
    -not -path '*/.git/*' -not -path '*/node_modules/*' \
    -not -path '*/.deno/*' -not -path '*/target/*' \
    -not -path '*/_build/*' -not -path '*/deps/*' \
    -not -path '*/external_corpora/*' -not -path '*/.lake/*' \
    -type f \( -name '*.rs' -o -name '*.ex' -o -name '*.exs' -o -name '*.res' \
      -o -name '*.js' -o -name '*.ts' -o -name '*.json' -o -name '*.toml' \
      -o -name '*.yml' -o -name '*.yaml' -o -name '*.md' -o -name '*.adoc' \
      -o -name '*.idr' -o -name '*.zig' -o -name '*.v' -o -name '*.jl' \
      -o -name '*.gleam' -o -name '*.hs' -o -name '*.ml' -o -name '*.sh' \) \
    -print0 > "$enumeration_file"; then
  echo "file enumeration failed: $scan_root" >&2
  exit 1
fi

while IFS= read -r -d '' filepath; do
  LC_ALL=C "$grep_bin" -aPq "$pattern" "$filepath"
  status=$?
  case "$status" in
    0)
      printf '%s\0' "$filepath" >> "$results_file" || scan_error=1
      if [[ -n "$blocking_results_file" ]]; then
        LC_ALL=C "$grep_bin" -aPq "$blocking_pattern" "$filepath"
        blocking_status=$?
        case "$blocking_status" in
          0) printf '%s\0' "$filepath" >> "$blocking_results_file" || scan_error=1 ;;
          1) ;;
          *) echo "blocking-classifier error ($blocking_status): $filepath" >&2; scan_error=1 ;;
        esac
      fi
      ;;
    1) ;;
    *) echo "scanner error ($status): $filepath" >&2; scan_error=1 ;;
  esac
done < "$enumeration_file"

exit "$scan_error"
