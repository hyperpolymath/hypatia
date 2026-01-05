# SPDX-License-Identifier: AGPL-3.0-or-later
# Standardize licenses across hyperpolymath repos

const OWNER = "hyperpolymath"
const LICENSE_TEMPLATE = "/tmp/LICENSE_TEMPLATE.txt"
const WORK_DIR = "/var/home/hyper/security-fixes"

# Skip these repos (have specific license requirements or are the license repo itself)
const SKIP_REPOS = [
    "palimpsest-license",  # The license repo itself
    "META.scm",            # Org-level metadata
    "ECOSYSTEM.scm",       # Org-level metadata
]

function get_all_repos()::Vector{String}
    cmd = Cmd(["gh", "repo", "list", OWNER, "--limit", "200", "--json", "name", "--jq", ".[].name"])
    output = read(cmd, String)
    repos = filter(!isempty, split(strip(output), '\n'))
    return filter(r -> !(r in SKIP_REPOS), repos)
end

function clone_or_pull(repo::String)::String
    repo_path = joinpath(WORK_DIR, repo)

    if isdir(repo_path)
        println("  Pulling latest...")
        cd(repo_path) do
            run(Cmd(["git", "pull", "--rebase"]))
        end
    else
        println("  Cloning...")
        run(Cmd(["git", "clone", "https://github.com/$OWNER/$repo.git", repo_path]))
    end

    return repo_path
end

function update_license(repo_path::String)::Bool
    license_dest = joinpath(repo_path, "LICENSE.txt")
    license_content = read(LICENSE_TEMPLATE, String)

    # Check if LICENSE.txt exists and is different
    if isfile(license_dest)
        existing = read(license_dest, String)
        if existing == license_content
            println("  License already up to date")
            return false
        end
    end

    # Also remove old LICENSE file if exists (prefer LICENSE.txt)
    old_license = joinpath(repo_path, "LICENSE")
    if isfile(old_license) && !isfile(license_dest)
        rm(old_license)
        println("  Removed old LICENSE file")
    end

    # Write new license
    write(license_dest, license_content)
    println("  Updated LICENSE.txt")
    return true
end

function commit_and_push(repo_path::String, repo::String)
    cd(repo_path) do
        # Check if there are changes
        status = read(Cmd(["git", "status", "--porcelain"]), String)
        if isempty(strip(status))
            println("  No changes to commit")
            return
        end

        # Stage LICENSE.txt
        run(Cmd(["git", "add", "LICENSE.txt"]))

        # Also remove old LICENSE from git if it was tracked
        try
            run(Cmd(["git", "rm", "-f", "LICENSE"]))
        catch
            # Ignore if LICENSE wasn't tracked
        end

        commit_msg = """chore(license): standardize to MIT OR AGPL-3.0-or-later

Dual license: MIT OR AGPL-3.0-or-later (your choice)
Includes Palimpsest philosophical overlay (encouraged, not required)

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>"""

        run(Cmd(["git", "commit", "-m", commit_msg]))
        run(Cmd(["git", "push"]))
        println("  Committed and pushed")
    end
end

function main()
    dry_run = "--dry-run" in ARGS
    single_repo = nothing

    for arg in ARGS
        if startswith(arg, "--repo=")
            single_repo = split(arg, "=")[2]
        end
    end

    if dry_run
        println("🔍 DRY RUN MODE - no changes will be made\n")
    end

    repos = isnothing(single_repo) ? get_all_repos() : [single_repo]

    println("=== Standardizing Licenses ===")
    println("Template: MIT OR AGPL-3.0-or-later + Palimpsest overlay")
    println("Repos to process: $(length(repos))\n")

    success_count = 0
    skip_count = 0
    fail_count = 0

    for repo in repos
        println("\n--- $repo ---")

        try
            repo_path = clone_or_pull(repo)

            if dry_run
                println("  [DRY RUN] Would update license")
                skip_count += 1
                continue
            end

            changed = update_license(repo_path)

            if changed
                commit_and_push(repo_path, repo)
                success_count += 1
            else
                skip_count += 1
            end
        catch e
            println("  ❌ Error: $e")
            fail_count += 1
        end
    end

    println("\n" * "="^50)
    println("SUMMARY")
    println("="^50)
    println("✅ Updated: $success_count")
    println("⏭️  Skipped (already current): $skip_count")
    println("❌ Failed: $fail_count")
end

main()
