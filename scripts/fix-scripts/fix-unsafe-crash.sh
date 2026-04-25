#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-unsafe-crash.sh — Report generic unsafe crash patterns (unsafe_crash-unknown)
# Recipe: recipe-unsafe-crash (confidence: 0.65, auto_fixable: false)
#
# Broad sweep for crash-inducing patterns not caught by specific detectors:
# abort(), process::exit(), std::process::abort, erlang:halt, System.halt.
#
# Usage: fix-unsafe-crash.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-unsafe-crash.sh <repo-path>}"
REPORTS=0

# Rust: process::abort / process::exit
while IFS= read -r -d '' file; do
  [[ "$file" == */target/* ]] && continue
  [[ "$file" == */.git/* ]] && continue

  count=$(grep -cP '\b(process::abort|process::exit|std::process::abort)\s*\(' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-unsafe-crash] Rust ${file}: ${count} hard-exit call(s)"
  grep -nP '\b(process::abort|process::exit|std::process::abort)\s*\(' "$file" 2>/dev/null | head -3
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f -name "*.rs" \
  -not -path "*/.git/*" -not -path "*/target/*" -print0 2>/dev/null)

# Elixir: :erlang.halt / System.halt
while IFS= read -r -d '' file; do
  [[ "$file" == */_build/* ]] && continue
  [[ "$file" == */.git/* ]] && continue

  count=$(grep -cP '(:erlang\.halt|System\.halt)\s*\(' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-unsafe-crash] Elixir ${file}: ${count} halt call(s)"
  grep -nP '(:erlang\.halt|System\.halt)\s*\(' "$file" 2>/dev/null | head -3
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f \( -name "*.ex" -o -name "*.exs" \) \
  -not -path "*/.git/*" -not -path "*/_build/*" -print0 2>/dev/null)

# C/C++: abort() / exit()
while IFS= read -r -d '' file; do
  [[ "$file" == */.git/* ]] && continue

  count=$(grep -cP '\b(abort|exit)\s*\(' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-unsafe-crash] C/C++ ${file}: ${count} abort/exit call(s)"
  grep -nP '\b(abort|exit)\s*\(' "$file" 2>/dev/null | head -3
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f \( -name "*.c" -o -name "*.cpp" -o -name "*.h" -o -name "*.hpp" \) \
  -not -path "*/.git/*" -print0 2>/dev/null)

# Shell: kill -9 $$ / exit 1 without context
while IFS= read -r -d '' file; do
  [[ "$file" == */.git/* ]] && continue

  count=$(grep -cP 'kill\s+-9\s+\$\$' "$file" 2>/dev/null || echo 0)
  [[ "$count" -eq 0 ]] && continue

  echo "[fix-unsafe-crash] Shell ${file}: ${count} kill -9 \$\$ pattern(s)"
  grep -nP 'kill\s+-9\s+\$\$' "$file" 2>/dev/null | head -3
  REPORTS=$((REPORTS + count))
done < <(find "$REPO" -type f \( -name "*.sh" -o -name "*.bash" \) \
  -not -path "*/.git/*" -print0 2>/dev/null)

echo "[fix-unsafe-crash] Total: ${REPORTS} unsafe crash pattern(s) reported"
