# SPDX-License-Identifier: AGPL-3.0-or-later
# Fix workflow permissions for repos missing `permissions: read-all`

using Dates

const REPOS = [
    "asdfghj",
    "robot-vacuum-cleaner",
    "blue-screen-of-app",
    "poly-secret-mcp",
    "poly-queue-mcp",
    "poly-observability-mcp",
    "poly-iac-mcp",
    "hackenbush-ssg",
    "ubicity"
]

const OWNER = "hyperpolymath"
const SPDX_HEADER = "# SPDX-License-Identifier: AGPL-3.0-or-later"
const PERMISSIONS_LINE = "permissions: read-all"

function fix_workflow_file(filepath::String)::Bool
    content = read(filepath, String)
    lines = split(content, '\n')
    modified = false
    new_lines = String[]

    has_spdx = any(l -> startswith(strip(l), "# SPDX-License-Identifier:"), lines)
    has_permissions = any(l -> startswith(strip(l), "permissions:"), lines)

    if has_permissions && has_spdx
        return false  # Already compliant
    end

    # Add SPDX header if missing
    if !has_spdx
        push!(new_lines, SPDX_HEADER)
        modified = true
    end

    # Process lines
    name_found = false
    permissions_added = false

    for (i, line) in enumerate(lines)
        stripped = strip(line)

        # Skip if this is where we'd add SPDX and we already added it
        if i == 1 && !has_spdx && startswith(stripped, "# SPDX")
            continue
        end

        push!(new_lines, line)

        # Add permissions after 'name:' line (top-level, not indented)
        if !has_permissions && !permissions_added && startswith(stripped, "name:") && !startswith(line, " ") && !startswith(line, "\t")
            name_found = true
        elseif name_found && !permissions_added && !isempty(stripped) && !startswith(stripped, "#")
            # Insert permissions before the next non-empty, non-comment line
            # But we need to insert BEFORE this line, so pop and re-add
            pop!(new_lines)
            push!(new_lines, "")
            push!(new_lines, PERMISSIONS_LINE)
            push!(new_lines, "")
            push!(new_lines, line)
            permissions_added = true
            modified = true
        end
    end

    # If we found name but never added permissions (file ended), add at end
    if name_found && !permissions_added && !has_permissions
        push!(new_lines, "")
        push!(new_lines, PERMISSIONS_LINE)
        modified = true
    end

    if modified
        # Write back
        open(filepath, "w") do f
            write(f, join(new_lines, '\n'))
        end
    end

    return modified
end

function fix_repo(repo::String; dry_run::Bool=false)
    repo_dir = joinpath(pwd(), repo)
    workflows_dir = joinpath(repo_dir, ".github", "workflows")

    println("\n" * "="^60)
    println("Processing: $OWNER/$repo")
    println("="^60)

    # Clone if not exists
    if !isdir(repo_dir)
        println("Cloning $repo...")
        run(`git clone --depth 1 https://github.com/$OWNER/$repo.git $repo_dir`)
    else
        println("Repo already cloned, pulling latest...")
        cd(repo_dir) do
            run(`git pull --ff-only`)
        end
    end

    if !isdir(workflows_dir)
        println("  No .github/workflows directory, skipping")
        return 0
    end

    # Find all workflow files
    workflow_files = filter(f -> endswith(f, ".yml") || endswith(f, ".yaml"), readdir(workflows_dir))

    if isempty(workflow_files)
        println("  No workflow files found")
        return 0
    end

    fixed_count = 0

    for wf in workflow_files
        filepath = joinpath(workflows_dir, wf)
        println("  Checking: $wf")

        if dry_run
            content = read(filepath, String)
            has_spdx = occursin(r"# SPDX-License-Identifier:", content)
            has_perms = occursin(r"^permissions:", content) || occursin(r"\npermissions:", content)
            if !has_spdx || !has_perms
                println("    Would fix: SPDX=$(!has_spdx), permissions=$(!has_perms)")
                fixed_count += 1
            else
                println("    Already compliant")
            end
        else
            if fix_workflow_file(filepath)
                println("    Fixed!")
                fixed_count += 1
            else
                println("    Already compliant")
            end
        end
    end

    # Commit and push if changes were made
    if fixed_count > 0 && !dry_run
        cd(repo_dir) do
            run(`git add -A`)
            commit_msg = """
            fix(security): add workflow permissions and SPDX headers

            - Added `permissions: read-all` to workflow files
            - Added SPDX-License-Identifier headers where missing
            - Addresses OpenSSF Scorecard Token-Permissions check

            🤖 Generated with [Claude Code](https://claude.ai/code)

            Co-Authored-By: Claude <noreply@anthropic.com>
            """
            try
                run(`git commit -m $commit_msg`)
                println("  Committed changes")
                run(`git push`)
                println("  Pushed to remote")
            catch e
                println("  Warning: commit/push failed - $(e)")
            end
        end
    end

    return fixed_count
end

function main()
    dry_run = "--dry-run" in ARGS
    single_repo = nothing

    for arg in ARGS
        if startswith(arg, "--repo=")
            single_repo = replace(arg, "--repo=" => "")
        end
    end

    if dry_run
        println("DRY RUN MODE - no changes will be made")
    end

    cd(dirname(@__FILE__))

    repos_to_fix = isnothing(single_repo) ? REPOS : [single_repo]
    total_fixed = 0

    for repo in repos_to_fix
        total_fixed += fix_repo(repo; dry_run=dry_run)
    end

    println("\n" * "="^60)
    println("SUMMARY: Fixed $total_fixed workflow files across $(length(repos_to_fix)) repos")
    println("="^60)
end

main()
