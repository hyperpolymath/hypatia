#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-hardcoded-secrets.sh — Detect hardcoded secrets in source code
# Recipe: recipe-fix-hardcoded-secrets (confidence: 0.90, auto_fixable: false)
#
# Reports potential hardcoded API keys, passwords, and tokens.
# Does not auto-fix — removal requires understanding each secret's context.
#
# Usage: fix-hardcoded-secrets.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-hardcoded-secrets.sh <repo-path>}"
REPORTS=0

# Secret patterns (high-confidence)
SECRET_PATTERNS=(
  'password\s*=\s*"[^"]{8,}'          # password = "..."
  'api_key\s*=\s*"[^"]{8,}'           # api_key = "..."
  'secret_key\s*=\s*"[^"]{8,}'        # secret_key = "..."
  'token\s*=\s*"[^"]{20,}'            # token = "..." (long string)
  'AKIA[0-9A-Z]{16}'                   # AWS Access Key ID
  'ghp_[0-9a-zA-Z]{36}'               # GitHub PAT (classic)
  'github_pat_[0-9a-zA-Z_]{82}'       # GitHub PAT (fine-grained)
  'sk-[0-9a-zA-Z]{48}'                # OpenAI API key
  'sk_live_[0-9a-zA-Z]{24,}'          # Stripe live key
  'xoxb-[0-9]+-[0-9]+-[0-9a-zA-Z]+'  # Slack bot token
  'Bearer\s+[0-9a-zA-Z\-_.]{20,}'     # Bearer token in string
)

while IFS= read -r -d '' file; do
  [[ "$file" == */.git/* ]] && continue
  [[ "$file" == */target/* ]] && continue
  [[ "$file" == */_build/* ]] && continue
  [[ "$file" == */.env.example ]] && continue  # Example env files are OK

  for pattern in "${SECRET_PATTERNS[@]}"; do
    matches=$(grep -cnP "$pattern" "$file" 2>/dev/null || echo 0)
    if [[ "$matches" -gt 0 ]]; then
      echo "[fix-hardcoded-secrets] ALERT: ${file} — ${matches} potential secret(s)"
      # Show line numbers but mask the actual values
      grep -nP "$pattern" "$file" 2>/dev/null | sed 's/=\s*"[^"]*"/= "***REDACTED***"/g' | head -3
      REPORTS=$((REPORTS + matches))
      break
    fi
  done
done < <(find "$REPO" -type f \
  \( -name "*.ex" -o -name "*.exs" -o -name "*.rs" -o -name "*.sh" \
     -o -name "*.toml" -o -name "*.json" -o -name "*.yml" -o -name "*.yaml" \
     -o -name "*.env" -o -name "*.res" -o -name "*.zig" -o -name "*.idr" \) \
  -not -path "*/.git/*" -not -path "*/target/*" -not -path "*/_build/*" \
  -not -name ".env.example" \
  -print0 2>/dev/null)

echo "[fix-hardcoded-secrets] Total: ${REPORTS} potential secret(s) reported"
