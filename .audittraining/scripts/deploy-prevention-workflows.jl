#!/usr/bin/env julia
# SPDX-License-Identifier: PMPL-1.0-or-later
# Deploy prevention workflows to all repos with open alerts

using JSON3
using Dates

const ORG = "hyperpolymath"
const PREVENTION_DIR = joinpath(@__DIR__, "..", "prevention")
const DRY_RUN = "--dry-run" in ARGS

# Workflows to deploy
const WORKFLOWS = [
    "workflow-linter.yml",
    "cargo-audit.yml",        # Only for Rust repos
    "secret-scanner.yml",
    "scorecard-enforcer.yml"
]

# Check if repo has Cargo.toml (is a Rust repo)
function is_rust_repo(repo::String)::Bool
    try
        run(pipeline(`gh api repos/$ORG/$repo/contents/Cargo.toml`, devnull))
        return true
    catch
        return false
    end
end

# Check if workflow already exists
function workflow_exists(repo::String, workflow::String)::Bool
    try
        run(pipeline(`gh api repos/$ORG/$repo/contents/.github/workflows/$workflow`, devnull))
        return true
    catch
        return false
    end
end

# Deploy workflow to repo
function deploy_workflow(repo::String, workflow::String)
    src_path = joinpath(PREVENTION_DIR, workflow)
    if !isfile(src_path)
        @warn "Workflow not found: $src_path"
        return false
    end

    content = read(src_path, String)
    content_b64 = base64encode(content)

    if DRY_RUN
        println("  [DRY-RUN] Would deploy $workflow to $repo")
        return true
    end

    # Create or update file via GitHub API
    try
        # Check if exists to get SHA
        existing_sha = nothing
        try
            output = read(`gh api repos/$ORG/$repo/contents/.github/workflows/$workflow --jq '.sha'`, String)
            existing_sha = strip(output)
        catch
        end

        message = "Add $workflow prevention workflow\n\n🤖 Generated with cicd-hyper-a"

        if isnothing(existing_sha)
            # Create new file
            run(`gh api repos/$ORG/$repo/contents/.github/workflows/$workflow -X PUT -f message=$message -f content=$content_b64`)
        else
            # Update existing
            run(`gh api repos/$ORG/$repo/contents/.github/workflows/$workflow -X PUT -f message=$message -f content=$content_b64 -f sha=$existing_sha`)
        end

        println("  ✓ Deployed $workflow")
        return true
    catch e
        @warn "Failed to deploy $workflow to $repo: $e"
        return false
    end
end

function main()
    if DRY_RUN
        println("🔄 DRY RUN MODE - No changes will be made")
    end

    println("📦 Deploying prevention workflows to hyperpolymath repos...")

    # Get repos with open alerts from cached data
    alerts_file = joinpath(@__DIR__, "..", "security-errors", "all_alerts.txt")

    if !isfile(alerts_file)
        # Fallback: get from GitHub API
        println("Fetching repos with alerts from GitHub...")
        repos_output = read(pipeline(
            `gh repo list $ORG --limit 300 --json name --jq '.[].name'`
        ), String)
        repos = filter(!isempty, split(repos_output, "\n"))
    else
        # Use cached list
        repos = unique([split(line, ":")[1] for line in readlines(alerts_file) if !isempty(line)])
    end

    println("Processing $(length(repos)) repos...")

    deployed = 0
    skipped = 0
    failed = 0

    for (i, repo) in enumerate(repos)
        println("\n[$i/$(length(repos))] $repo")

        rust_repo = is_rust_repo(repo)

        for workflow in WORKFLOWS
            # Skip cargo-audit for non-Rust repos
            if workflow == "cargo-audit.yml" && !rust_repo
                continue
            end

            # Skip if already exists
            if workflow_exists(repo, workflow)
                println("  - $workflow (already exists)")
                skipped += 1
                continue
            end

            if deploy_workflow(repo, workflow)
                deployed += 1
            else
                failed += 1
            end
        end
    end

    println("\n📊 Summary:")
    println("  Deployed: $deployed")
    println("  Skipped: $skipped")
    println("  Failed: $failed")
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
