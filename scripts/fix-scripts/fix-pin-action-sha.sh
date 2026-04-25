#!/usr/bin/env bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# fix-pin-action-sha.sh — Pin GitHub Actions to SHA hashes (canonical list + GH API fallback)
# Recipe: recipe-pin-dependencies (confidence: 0.90, auto_fixable: true)
#
# Superset of fix-unpinned-actions.sh: applies canonical SHA pins from the
# hyperpolymath CLAUDE.md standard, then resolves any remaining tag-pinned
# actions via the GitHub API. Requires 'gh' CLI and auth for the second pass;
# falls back to report-only if gh is unavailable.
#
# Usage: fix-pin-action-sha.sh <repo-path>

set -euo pipefail

REPO="${1:?Usage: fix-pin-action-sha.sh <repo-path>}"
WORKFLOWS_DIR="${REPO}/.github/workflows"
FIXES=0
UNRESOLVED=0

[[ -d "$WORKFLOWS_DIR" ]] || { echo "[fix-pin-action-sha] No workflows dir"; exit 0; }

# Canonical SHA pins — from hyperpolymath CLAUDE.md (updated 2026-04-17)
declare -A SHA_PINS=(
  ["actions/checkout@v4"]="34e114876b0b11c390a56381ad16ebd13914f8d5"
  ["actions/checkout@v5"]="93cb6efe18208431cddfb8368fd83d5badbf9bfd"
  ["actions/checkout@v6.0.2"]="de0fac2e4500dabe0009e67214ff5f5447ce83dd"
  ["github/codeql-action@v3"]="6624720a57d4c312633c7b953db2f2da5bcb4c3a"
  ["ossf/scorecard-action@v2.4.0"]="62b2cac7ed8198b15735ed49ab1e5cf35480ba46"
  ["dtolnay/rust-toolchain@stable"]="4be9e76fd7c4901c61fb841f559994984270fce7"
  ["Swatinem/rust-cache@v2"]="779680da715d629ac1d338a641029a2f4372abb5"
  ["codecov/codecov-action@v5"]="671740ac38dd9b0130fbe1cec585b89eea48d3de"
  ["trufflesecurity/trufflehog@main"]="7ee2e0fdffec27d19ccbb8fb3dcf8a83b9d7f9e8"
  ["editorconfig-checker/action-editorconfig-checker@main"]="4054fa83a075fdf090bd098bdb1c09aaf64a4169"
  ["slsa-framework/slsa-github-generator@v2.1.0"]="f7dd8c54c2067bafc12ca7a55595d5ee9b75204a"
  ["webfactory/ssh-agent@v0.9.0"]="dc588b651fe13675774614f8e6a936a468676387"
  ["ocaml/setup-ocaml@v3"]="dec6499fef64fc5d7ed43d43a87251b7b1c306f5"
  ["softprops/action-gh-release@v2"]="a06a81a03ee405af7f2048a818ed3f03bbf83c7b"
  ["actions/configure-pages@v5"]="983d7736d9b0ae728b81ab479565c72886d7745b"
  ["actions/jekyll-build-pages@v1"]="44a6e6beabd48582f863aeeb6cb2151cc1716697"
  ["actions/upload-pages-artifact@v3"]="56afc609e74202658d3ffba0e8f6dda462b719fa"
  ["actions/deploy-pages@v4"]="d6db90164ac5ed86f2b6aed7e0febac5b3c0c03e"
  ["ruby/setup-ruby@v1"]="09a7688d3b55cf0e976497ff046b70949eeaccfd"
  ["erlef/setup-beam@v1"]="ee09b1e59bb240681c382eb1f0abc6a04af72764"
  ["actions/upload-artifact@v4"]="bbbca2ddaa5d8feaa63e36b76fdaad77386f024f"
  ["actions/github-script@v7"]="ed597411d8f924073f98dfc5c65a23a2325f34cd"
)

GH_AVAILABLE=false
if command -v gh &>/dev/null && gh auth status &>/dev/null 2>&1; then
  GH_AVAILABLE=true
fi

# Resolve a tag/branch ref to a commit SHA via GitHub API.
# Handles both lightweight tags (direct commit) and annotated tags (deref needed).
resolve_to_sha() {
  local owner_repo="$1"
  local ref="$2"
  local sha=""

  # Try commits API — returns the commit SHA for any ref (tag or branch HEAD)
  sha=$(gh api "repos/${owner_repo}/commits/${ref}" --jq '.sha' 2>/dev/null || echo "")

  echo "$sha"
}

# Pass 1: apply canonical SHA pins (offline, no API needed)
for wf in "${WORKFLOWS_DIR}"/*.yml "${WORKFLOWS_DIR}"/*.yaml; do
  [[ -f "$wf" ]] || continue

  for action_ref in "${!SHA_PINS[@]}"; do
    sha="${SHA_PINS[$action_ref]}"
    action_name="${action_ref%%@*}"
    version="${action_ref##*@}"

    if grep -q "uses:.*${action_ref}" "$wf" 2>/dev/null; then
      if ! grep -q "uses:.*${action_name}@[0-9a-f]\{40\}" "$wf" 2>/dev/null; then
        sed -i "s|${action_ref}|${action_name}@${sha} # ${version}|g" "$wf"
        echo "[fix-pin-action-sha] Pinned ${action_ref} → ${sha:0:12}... in $(basename "$wf")"
        FIXES=$((FIXES + 1))
      fi
    fi
  done
done

# Pass 2: handle remaining tag-pinned actions via GH API (non-canonical)
for wf in "${WORKFLOWS_DIR}"/*.yml "${WORKFLOWS_DIR}"/*.yaml; do
  [[ -f "$wf" ]] || continue

  while IFS= read -r action_ref; do
    [[ -z "$action_ref" ]] && continue

    # Already SHA-pinned (40 hex chars after @) — skip
    if echo "$action_ref" | grep -qE '@[0-9a-f]{40}'; then
      continue
    fi

    action_name="${action_ref%%@*}"
    ref="${action_ref##*@}"

    # Skip actions with no ref separator
    [[ "$action_name" == "$action_ref" ]] && continue

    # Skip local/composite actions
    [[ "$action_name" == .* ]] && continue

    # Skip Docker images (docker://)
    [[ "$action_ref" == docker://* ]] && continue

    if [[ "$GH_AVAILABLE" == "true" ]]; then
      sha=$(resolve_to_sha "$action_name" "$ref")
      if [[ -n "$sha" && ${#sha} -eq 40 ]]; then
        sed -i "s|${action_ref}|${action_name}@${sha} # ${ref}|g" "$wf"
        echo "[fix-pin-action-sha] Pinned ${action_ref} → ${sha:0:12}... in $(basename "$wf")"
        FIXES=$((FIXES + 1))
      else
        echo "[fix-pin-action-sha] WARN: Could not resolve ${action_ref} to commit SHA" >&2
        UNRESOLVED=$((UNRESOLVED + 1))
      fi
    else
      echo "[fix-pin-action-sha] REPORT (gh unavailable): unpinned ${action_ref} in $(basename "$wf")" >&2
      UNRESOLVED=$((UNRESOLVED + 1))
    fi
  done < <(
    grep -oE 'uses:[[:space:]]+[^[:space:]#]+' "$wf" 2>/dev/null \
      | sed 's/uses:[[:space:]]*//' \
      || true
  )
done

echo "[fix-pin-action-sha] Total: ${FIXES} action(s) pinned, ${UNRESOLVED} unresolved"
