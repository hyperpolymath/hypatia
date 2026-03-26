#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-license-file.sh — Add LICENSE file with PMPL-1.0-or-later
# Recipe: recipe-add-license-file (confidence: 0.95, auto_fixable: true)
#
# Usage: fix-license-file.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-license-file.sh <repo-path>}"

# Check for existing license files
for name in LICENSE LICENSE.txt LICENSE.md LICENCE COPYING; do
  if [[ -f "${REPO}/${name}" ]]; then
    echo "[fix-license-file] Already exists: ${REPO}/${name}"
    exit 0
  fi
done

# Use the template license if available
TEMPLATE_LICENSE="${HOME}/Documents/hyperpolymath-repos/rsr-template-repo/LICENSE"
if [[ -f "$TEMPLATE_LICENSE" ]]; then
  cp "$TEMPLATE_LICENSE" "${REPO}/LICENSE"
  echo "[fix-license-file] Copied PMPL-1.0-or-later LICENSE from template"
else
  # Create a minimal placeholder
  cat > "${REPO}/LICENSE" <<'EOF'
Palimpsest License (PMPL-1.0-or-later)

SPDX-License-Identifier: PMPL-1.0-or-later

Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

This software is licensed under the Palimpsest License, Version 1.0 or later.

For the full license text, see:
https://github.com/hyperpolymath/palimpsest-license
EOF
  echo "[fix-license-file] Created placeholder LICENSE (PMPL-1.0-or-later)"
fi
