# SPDX-License-Identifier: AGPL-3.0-or-later
# Enable branch protection across hyperpolymath repos

using JSON3

const OWNER = "hyperpolymath"

# Repos that had BranchProtectionID scorecard alerts
const PRIORITY_REPOS = [
    "thejeffparadox",
    "bunsenite",
    "asdfghj",
    "robot-vacuum-cleaner",
    "blue-screen-of-app",
    "poly-secret-mcp",
    "poly-queue-mcp",
    "poly-observability-mcp",
    "poly-iac-mcp",
    "hackenbush-ssg",
    "ubicity",
    "supernorma",
    "polyglot-i18n",
    "rhodium-standard-repositories",
    "git-eco-bot",
    "czech-file-knife",
    "echidnabot"
]

# Branch protection settings (GitHub API format)
const PROTECTION_SETTINGS = """
{
  "required_status_checks": null,
  "enforce_admins": false,
  "required_pull_request_reviews": {
    "dismiss_stale_reviews": false,
    "require_code_owner_reviews": false,
    "required_approving_review_count": 0
  },
  "restrictions": null,
  "allow_force_pushes": false,
  "allow_deletions": false,
  "block_creations": false,
  "required_conversation_resolution": false,
  "lock_branch": false,
  "allow_fork_syncing": true
}
"""

function get_default_branch(repo::String)::String
    output = read(`gh api repos/$OWNER/$repo --jq .default_branch`, String)
    return strip(output)
end

function has_branch_protection(repo::String, branch::String)::Bool
    try
        run(pipeline(`gh api repos/$OWNER/$repo/branches/$branch/protection`, devnull))
        return true
    catch
        return false
    end
end

function enable_branch_protection(repo::String, branch::String; dry_run::Bool=false)
    println("  Enabling protection for $repo ($branch)...")

    if dry_run
        println("    [DRY RUN] Would enable branch protection")
        return true
    end

    # Write settings to temp file
    settings_file = tempname() * ".json"
    open(settings_file, "w") do f
        write(f, PROTECTION_SETTINGS)
    end

    try
        run(`gh api -X PUT repos/$OWNER/$repo/branches/$branch/protection --input $settings_file`)
        println("    ✅ Protection enabled")
        return true
    catch e
        println("    ❌ Failed: $e")
        return false
    finally
        rm(settings_file, force=true)
    end
end

function main()
    dry_run = "--dry-run" in ARGS
    all_repos = "--all" in ARGS

    if dry_run
        println("🔍 DRY RUN MODE - no changes will be made\n")
    end

    repos = all_repos ? get_all_repos() : PRIORITY_REPOS

    println("=== Enabling Branch Protection ===")
    println("Repos to process: $(length(repos))\n")

    success_count = 0
    skip_count = 0
    fail_count = 0

    for repo in repos
        println("\n--- $repo ---")

        try
            branch = get_default_branch(repo)
            println("  Default branch: $branch")

            if has_branch_protection(repo, branch)
                println("  ⏭️  Already protected, skipping")
                skip_count += 1
                continue
            end

            if enable_branch_protection(repo, branch; dry_run=dry_run)
                success_count += 1
            else
                fail_count += 1
            end
        catch e
            println("  ❌ Error: $e")
            fail_count += 1
        end
    end

    println("\n" * "="^50)
    println("SUMMARY")
    println("="^50)
    println("✅ Protected: $success_count")
    println("⏭️  Skipped (already protected): $skip_count")
    println("❌ Failed: $fail_count")
end

function get_all_repos()::Vector{String}
    output = read(`gh repo list $OWNER --limit 200 --json name --jq .[].name`, String)
    return filter(!isempty, split(strip(output), '\n'))
end

main()
