#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-security-policy.sh — Add SECURITY.md vulnerability disclosure policy
# Recipe: recipe-add-security-policy (confidence: 0.95, auto_fixable: true)
#
# Usage: fix-security-policy.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-security-policy.sh <repo-path>}"
TARGET="${REPO}/SECURITY.md"

if [[ -f "$TARGET" ]]; then
  echo "[fix-security-policy] Already exists: ${TARGET}"
  exit 0
fi

# Derive repo name from directory
REPO_NAME="$(basename "$REPO")"

cat > "$TARGET" <<EOF
# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| latest  | :white_check_mark: |

## Reporting a Vulnerability

If you discover a security vulnerability in ${REPO_NAME}, please report it
responsibly:

1. **Do NOT open a public issue.**
2. Email: j.d.a.jewell@open.ac.uk
3. Include:
   - Description of the vulnerability
   - Steps to reproduce
   - Potential impact
   - Suggested fix (if any)

You should receive a response within 48 hours. We will work with you to
understand and address the issue before any public disclosure.

## Security Scanning

This repository is scanned by:
- [Hypatia](https://github.com/hyperpolymath/hypatia) — Neurosymbolic CI/CD security
- [TruffleHog](https://github.com/trufflesecurity/trufflehog) — Secret detection
- [OpenSSF Scorecard](https://securityscorecards.dev/) — Supply chain security
EOF

echo "[fix-security-policy] Created ${TARGET}"
