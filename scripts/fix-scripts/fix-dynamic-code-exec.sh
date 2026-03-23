#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-dynamic-code-exec.sh — Report dynamic code execution patterns
# Recipe: recipe-dynamic-apply-to-dispatch (confidence: 0.65, auto_fixable: false)
#
# Reports apply/3, Module.concat, and dynamic module calls in Elixir.
#
# Usage: fix-dynamic-code-exec.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-dynamic-code-exec.sh <repo-path>}"
REPORTS=0

while IFS= read -r -d '' file; do
  [[ "$file" == */_build/* ]] && continue
  [[ "$file" == */deps/* ]] && continue

  count=$(grep -cP '(apply\(|Kernel\.apply|Module\.concat|Code\.eval_string)' "$file" 2>/dev/null || echo 0)
  if [[ "$count" -gt 0 ]]; then
    echo "[fix-dynamic-code-exec] REPORT: ${file} — ${count} dynamic dispatch(es)"
    grep -nP '(apply\(|Kernel\.apply|Module\.concat|Code\.eval_string)' "$file" 2>/dev/null | head -5
    REPORTS=$((REPORTS + count))
  fi
done < <(find "$REPO" -type f \( -name "*.ex" -o -name "*.exs" \) \
  -not -path "*/.git/*" -not -path "*/_build/*" -not -path "*/deps/*" \
  -print0 2>/dev/null)

echo "[fix-dynamic-code-exec] Total: ${REPORTS} dynamic dispatch(es) reported"
