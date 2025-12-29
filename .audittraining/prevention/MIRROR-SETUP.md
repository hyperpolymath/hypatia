# Mirror Setup Instructions

## Required Secrets (Org-level)

Set these in GitHub org settings → Secrets → Actions:

| Secret | Purpose |
|--------|---------|
| `GITLAB_SSH_KEY` | SSH private key for gitlab.com/hyperpolymath |
| `BITBUCKET_SSH_KEY` | SSH private key for bitbucket.org/hyperpolymath |
| `CODEBERG_SSH_KEY` | SSH private key for codeberg.org/hyperpolymath |
| `SOURCEHUT_SSH_KEY` | SSH private key for git.sr.ht/~hyperpolymath |

## Required Variables (Org-level)

Set these in GitHub org settings → Variables → Actions:

| Variable | Value |
|----------|-------|
| `GITLAB_MIRROR_ENABLED` | `true` |
| `BITBUCKET_MIRROR_ENABLED` | `true` |
| `CODEBERG_MIRROR_ENABLED` | `true` |
| `SOURCEHUT_MIRROR_ENABLED` | `true` |

## SSH Key Setup

1. Generate keys: `ssh-keygen -t ed25519 -C "github-mirror"`
2. Add public key to each forge's SSH settings
3. Add private key to GitHub secrets

## Pre-create repos on forges

Before mirroring works, repos must exist on target forges.
Use the bulk creation scripts in this directory.
