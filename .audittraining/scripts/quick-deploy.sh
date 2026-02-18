#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Quick deploy prevention workflows to repos with most alerts

set -e

ORG="hyperpolymath"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PREVENTION_DIR="$SCRIPT_DIR/../prevention"
DRY_RUN="${1:-}"

# Workflows to deploy
WORKFLOWS=(
    "workflow-linter.yml"
    "secret-scanner.yml"
    "scorecard-enforcer.yml"
)

# Top repos with most alerts (30 each)
HIGH_PRIORITY_REPOS=(
    "rhodium-standard-repositories"
    "polyglot-i18n"
    "git-secure"
    "gitloom"
    "git-dispatcher"
    "ubicity"
    "supernorma"
    "bunsenite"
    "czech-file-knife"
    "proof-of-work"
)

deploy_workflow() {
    local repo=$1
    local workflow=$2
    local src="$PREVENTION_DIR/$workflow"

    if [ ! -f "$src" ]; then
        echo "  ⚠ Workflow not found: $src"
        return 1
    fi

    # Check if exists
    if gh api "repos/$ORG/$repo/contents/.github/workflows/$workflow" &>/dev/null; then
        echo "  - $workflow (exists)"
        return 0
    fi

    if [ "$DRY_RUN" = "--dry-run" ]; then
        echo "  [DRY-RUN] Would deploy $workflow"
        return 0
    fi

    # Deploy
    local content
    content=$(base64 < "$src")
    local message="Add $workflow prevention workflow

🤖 Generated with cicd-hyper-a

Co-Authored-By: Claude Opus 4.5 <noreply@anthropic.com>"

    if gh api "repos/$ORG/$repo/contents/.github/workflows/$workflow" \
        -X PUT \
        -f message="$message" \
        -f content="$content" &>/dev/null; then
        echo "  ✓ Deployed $workflow"
        return 0
    else
        echo "  ✗ Failed $workflow"
        return 1
    fi
}

main() {
    echo "🚀 Deploying prevention workflows to high-priority repos..."
    [ "$DRY_RUN" = "--dry-run" ] && echo "   (DRY RUN MODE)"

    local deployed=0
    local skipped=0
    local failed=0

    for repo in "${HIGH_PRIORITY_REPOS[@]}"; do
        echo ""
        echo "📦 $repo"

        for workflow in "${WORKFLOWS[@]}"; do
            if deploy_workflow "$repo" "$workflow"; then
                ((deployed++)) || true
            else
                ((failed++)) || true
            fi
        done
    done

    echo ""
    echo "📊 Summary: deployed=$deployed failed=$failed"
}

main "$@"
