# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# Batch Repository Operations for .git-private-farm Integration
# Provides checklist-based operations across multiple repos
#
# Usage:
#   julia repo-batch-ops.jl --list-repos           # Show all repos with metadata
#   julia repo-batch-ops.jl --select               # Interactive selection
#   julia repo-batch-ops.jl --checklist            # Show available operations
#   julia repo-batch-ops.jl --run <operation>      # Run operation on selected repos
#   julia repo-batch-ops.jl --dry-run <operation>  # Preview without changes
#

using Dates
using JSON

# =============================================================================
# Configuration
# =============================================================================

const GITHUB_ORG = "hyperpolymath"
const REPOS_DIR = expanduser("~/repos")
const SELECTION_FILE = expanduser("~/.git-private-farm/selected-repos.json")
const OPERATIONS_LOG = expanduser("~/.git-private-farm/operations.log")

# =============================================================================
# Repository Selection Criteria
# =============================================================================

"""
Repository metadata for filtering and selection
"""
mutable struct RepoMeta
    name::String
    path::String
    languages::Vector{String}
    ecosystem::String
    type::String           # "library", "application", "ssg", "mcp", "spec", etc.
    has_jekyll::Bool
    has_typescript::Bool
    has_python::Bool
    has_go::Bool
    has_ruby::Bool
    has_makefile::Bool
    missing_scm::Vector{String}
    missing_workflows::Vector{String}
    last_commit::String
    branch::String
end

# =============================================================================
# Available Operations Checklist
# =============================================================================

const OPERATIONS = Dict(
    # Structure Operations
    "structure-init" => (
        name = "Initialize RSR Structure",
        description = "Create .machine_readable/, tasks/, licenses/ directories",
        category = "structure"
    ),
    "move-scm" => (
        name = "Move SCM Files",
        description = "Move *.scm from root to .machine_readable/6scm/",
        category = "structure"
    ),
    "move-build" => (
        name = "Move Build Files",
        description = "Move Justfile/Mustfile to tasks/",
        category = "structure"
    ),
    "create-licenses" => (
        name = "Create License Structure",
        description = "Add licenses/ with MPL-2.0 and Palimpsest in EN/NL",
        category = "structure"
    ),

    # Cleanup Operations
    "remove-jekyll" => (
        name = "Remove Jekyll",
        description = "Delete Jekyll workflows, Gemfile, _config.yml",
        category = "cleanup"
    ),
    "remove-typescript" => (
        name = "Remove TypeScript",
        description = "Flag/remove *.ts files (requires migration)",
        category = "cleanup"
    ),
    "remove-python" => (
        name = "Remove Python",
        description = "Flag/remove *.py files (requires migration)",
        category = "cleanup"
    ),
    "remove-go" => (
        name = "Remove Go",
        description = "Flag/remove *.go files (requires migration)",
        category = "cleanup"
    ),
    "remove-makefile" => (
        name = "Remove Makefiles",
        description = "Delete Makefile, replace with Justfile",
        category = "cleanup"
    ),

    # Workflow Operations
    "add-workflows" => (
        name = "Add Standard Workflows",
        description = "Add/update all RSR-compliant workflows",
        category = "workflows"
    ),
    "fix-permissions" => (
        name = "Fix Workflow Permissions",
        description = "Add 'permissions: read-all' to all workflows",
        category = "workflows"
    ),
    "pin-actions" => (
        name = "Pin GitHub Actions",
        description = "Replace version tags with SHA pins",
        category = "workflows"
    ),
    "add-spdx" => (
        name = "Add SPDX Headers",
        description = "Add SPDX-License-Identifier to all files",
        category = "workflows"
    ),
    "fix-codeql" => (
        name = "Fix CodeQL Languages",
        description = "Set CodeQL matrix to actual repo languages",
        category = "workflows"
    ),

    # Documentation Operations
    "add-security" => (
        name = "Add SECURITY.md",
        description = "Add security policy document",
        category = "docs"
    ),
    "add-contributing" => (
        name = "Add CONTRIBUTING.md",
        description = "Add contribution guidelines",
        category = "docs"
    ),
    "update-readme" => (
        name = "Update README badges",
        description = "Add/update OpenSSF, license badges",
        category = "docs"
    ),

    # Sync Operations
    "mirror-setup" => (
        name = "Setup Mirroring",
        description = "Configure GitLab/Bitbucket mirror vars",
        category = "sync"
    ),
    "push-all" => (
        name = "Push All Changes",
        description = "Git push to origin for all selected repos",
        category = "sync"
    ),
    "sync-diverged" => (
        name = "Sync Diverged Repos",
        description = "Resolve diverged branches with upstream",
        category = "sync"
    )
)

# =============================================================================
# Repository Detection Functions
# =============================================================================

"""
Detect languages in a repository
"""
function detect_languages(repo_path::String)::Vector{String}
    languages = String[]

    # Check for language indicators
    indicators = Dict(
        "rust" => ["Cargo.toml"],
        "rescript" => ["rescript.json", "bsconfig.json"],
        "deno" => ["deno.json", "deno.jsonc"],
        "gleam" => ["gleam.toml"],
        "ocaml" => ["dune-project"],
        "haskell" => [".cabal"],
        "elixir" => ["mix.exs"],
        "nickel" => ["*.ncl"],
        "julia" => ["Project.toml"],
        "typescript" => ["tsconfig.json"],
        "python" => ["pyproject.toml", "setup.py", "requirements.txt"],
        "go" => ["go.mod"],
        "ruby" => ["Gemfile"]
    )

    for (lang, files) in indicators
        for file in files
            if contains(file, "*")
                pattern = replace(file, "*" => "")
                matches = filter(f -> endswith(f, pattern), readdir(repo_path))
                if !isempty(matches)
                    push!(languages, lang)
                    break
                end
            elseif isfile(joinpath(repo_path, file))
                push!(languages, lang)
                break
            end
        end
    end

    unique(languages)
end

"""
Detect repository type
"""
function detect_type(repo_path::String, name::String)::String
    if endswith(name, "-ssg")
        return "ssg"
    elseif endswith(name, "-mcp")
        return "mcp"
    elseif contains(name, "spec") || contains(name, "standard")
        return "spec"
    elseif isfile(joinpath(repo_path, "Cargo.toml"))
        cargo = read(joinpath(repo_path, "Cargo.toml"), String)
        if contains(cargo, "[[bin]]")
            return "application"
        else
            return "library"
        end
    elseif isdir(joinpath(repo_path, "src"))
        return "library"
    else
        return "unknown"
    end
end

"""
Check for banned language artifacts
"""
function check_banned(repo_path::String)
    has_ts = !isempty(filter(f -> endswith(f, ".ts") || endswith(f, ".tsx"),
                              collect(walkdir(repo_path) |> x -> vcat([f for (r,d,fs) in x for f in fs]...))))
    has_py = !isempty(filter(f -> endswith(f, ".py"),
                              collect(walkdir(repo_path) |> x -> vcat([f for (r,d,fs) in x for f in fs]...))))
    has_go = isfile(joinpath(repo_path, "go.mod"))
    has_rb = isfile(joinpath(repo_path, "Gemfile"))
    has_make = isfile(joinpath(repo_path, "Makefile")) || isfile(joinpath(repo_path, "makefile"))

    (has_ts, has_py, has_go, has_rb, has_make)
end

"""
Check for Jekyll presence
"""
function has_jekyll(repo_path::String)::Bool
    jekyll_indicators = [
        "_config.yml",
        joinpath(".github", "workflows", "jekyll.yml"),
        joinpath(".github", "workflows", "jekyll-gh-pages.yml")
    ]

    any(f -> isfile(joinpath(repo_path, f)), jekyll_indicators)
end

"""
Check for missing SCM files
"""
function check_missing_scm(repo_path::String)::Vector{String}
    scm_dir = joinpath(repo_path, ".machine_readable", "6scm")
    required_scm = ["META.scm", "STATE.scm", "ECOSYSTEM.scm", "PLAYBOOK.scm", "AGENTIC.scm", "NEUROSYM.scm"]

    if !isdir(scm_dir)
        return required_scm
    end

    filter(f -> !isfile(joinpath(scm_dir, f)), required_scm)
end

"""
Scan repository and build metadata
"""
function scan_repo(repo_path::String)::Union{RepoMeta, Nothing}
    if !isdir(joinpath(repo_path, ".git"))
        return nothing
    end

    name = basename(repo_path)
    languages = detect_languages(repo_path)
    type = detect_type(repo_path, name)
    jekyll = has_jekyll(repo_path)
    ts, py, go, rb, make = check_banned(repo_path)
    missing_scm = check_missing_scm(repo_path)

    # Get last commit info
    last_commit = try
        strip(read(`git -C $repo_path log -1 --format=%ci`, String))
    catch
        ""
    end

    branch = try
        strip(read(`git -C $repo_path branch --show-current`, String))
    catch
        "unknown"
    end

    # Detect ecosystem from ECOSYSTEM.scm if present
    ecosystem = "unknown"
    eco_file = joinpath(repo_path, ".machine_readable", "6scm", "ECOSYSTEM.scm")
    if isfile(eco_file)
        content = read(eco_file, String)
        m = match(r"type\s+\"([^\"]+)\"", content)
        if m !== nothing
            ecosystem = m[1]
        end
    end

    RepoMeta(
        name, repo_path, languages, ecosystem, type,
        jekyll, ts, py, go, rb, make,
        missing_scm, String[], last_commit, branch
    )
end

# =============================================================================
# Selection Functions
# =============================================================================

"""
Filter repos by criteria
"""
function filter_repos(repos::Vector{RepoMeta};
                      language::String="",
                      type::String="",
                      ecosystem::String="",
                      has_banned::Bool=false,
                      has_jekyll::Bool=false,
                      missing_scm::Bool=false)

    filtered = repos

    if !isempty(language)
        filtered = filter(r -> language in r.languages, filtered)
    end

    if !isempty(type)
        filtered = filter(r -> r.type == type, filtered)
    end

    if !isempty(ecosystem)
        filtered = filter(r -> r.ecosystem == ecosystem, filtered)
    end

    if has_banned
        filtered = filter(r -> r.has_typescript || r.has_python || r.has_go || r.has_ruby || r.has_makefile, filtered)
    end

    if has_jekyll
        filtered = filter(r -> r.has_jekyll, filtered)
    end

    if missing_scm
        filtered = filter(r -> !isempty(r.missing_scm), filtered)
    end

    filtered
end

"""
Save selection to file
"""
function save_selection(repos::Vector{String})
    mkpath(dirname(SELECTION_FILE))
    open(SELECTION_FILE, "w") do f
        JSON.print(f, Dict("repos" => repos, "timestamp" => string(now())), 2)
    end
    println("Saved $(length(repos)) repos to selection")
end

"""
Load selection from file
"""
function load_selection()::Vector{String}
    if !isfile(SELECTION_FILE)
        return String[]
    end
    data = JSON.parsefile(SELECTION_FILE)
    get(data, "repos", String[])
end

# =============================================================================
# Operation Execution
# =============================================================================

"""
Execute operation on selected repos
"""
function run_operation(operation::String, repos::Vector{String}; dry_run::Bool=false)
    if !haskey(OPERATIONS, operation)
        println("Unknown operation: $operation")
        println("Available operations:")
        list_operations()
        return
    end

    op = OPERATIONS[operation]
    println("="^60)
    println("Operation: $(op.name)")
    println("Description: $(op.description)")
    println("Repos: $(length(repos))")
    println("Mode: $(dry_run ? "DRY RUN" : "LIVE")")
    println("="^60)

    for repo in repos
        repo_path = joinpath(REPOS_DIR, repo)
        if !isdir(repo_path)
            println("SKIP: $repo (not found)")
            continue
        end

        println("\n>>> Processing: $repo")

        if dry_run
            println("  Would execute: $operation")
            continue
        end

        # Execute operation
        try
            execute_operation(operation, repo_path)
            log_operation(repo, operation, "success")
        catch e
            println("  ERROR: $e")
            log_operation(repo, operation, "failed: $e")
        end
    end
end

"""
Execute specific operation on a repo
"""
function execute_operation(operation::String, repo_path::String)
    if operation == "structure-init"
        mkpath(joinpath(repo_path, ".machine_readable", "6scm"))
        mkpath(joinpath(repo_path, ".machine_readable", "anchoring"))
        mkpath(joinpath(repo_path, ".machine_readable", "reproducibility"))
        mkpath(joinpath(repo_path, ".machine_readable", "config"))
        mkpath(joinpath(repo_path, "tasks"))
        mkpath(joinpath(repo_path, "licenses"))
        mkpath(joinpath(repo_path, "docs", "machines"))
        println("  Created RSR directory structure")

    elseif operation == "move-scm"
        scm_dest = joinpath(repo_path, ".machine_readable", "6scm")
        mkpath(scm_dest)
        for scm in filter(f -> endswith(f, ".scm"), readdir(repo_path))
            src = joinpath(repo_path, scm)
            dst = joinpath(scm_dest, scm)
            if isfile(src)
                mv(src, dst, force=true)
                println("  Moved $scm to .machine_readable/6scm/")
            end
        end

    elseif operation == "move-build"
        tasks_dir = joinpath(repo_path, "tasks")
        mkpath(tasks_dir)
        for build_file in ["Justfile", "Mustfile", "justfile", "mustfile"]
            src = joinpath(repo_path, build_file)
            if isfile(src)
                dst = joinpath(tasks_dir, uppercasefirst(build_file))
                mv(src, dst, force=true)
                println("  Moved $build_file to tasks/")
            end
        end

    elseif operation == "remove-jekyll"
        # Remove Jekyll config
        for f in ["_config.yml", "Gemfile", "Gemfile.lock", ".ruby-version"]
            path = joinpath(repo_path, f)
            if isfile(path)
                rm(path)
                println("  Removed $f")
            end
        end
        # Remove Jekyll workflows
        for wf in ["jekyll.yml", "jekyll-gh-pages.yml"]
            path = joinpath(repo_path, ".github", "workflows", wf)
            if isfile(path)
                rm(path)
                println("  Removed .github/workflows/$wf")
            end
        end
        # Remove _site, _layouts, _includes, _posts
        for d in ["_site", "_layouts", "_includes", "_posts", "_data"]
            path = joinpath(repo_path, d)
            if isdir(path)
                rm(path, recursive=true)
                println("  Removed $d/")
            end
        end

    elseif operation == "remove-makefile"
        for mf in ["Makefile", "makefile", "GNUmakefile"]
            path = joinpath(repo_path, mf)
            if isfile(path)
                rm(path)
                println("  Removed $mf")
            end
        end
        # Also remove .mk files
        for f in filter(f -> endswith(f, ".mk"), readdir(repo_path))
            rm(joinpath(repo_path, f))
            println("  Removed $f")
        end

    elseif operation == "push-all"
        run(`git -C $repo_path push origin`)
        println("  Pushed to origin")

    else
        println("  Operation not yet implemented: $operation")
    end
end

"""
Log operation to file
"""
function log_operation(repo::String, operation::String, result::String)
    mkpath(dirname(OPERATIONS_LOG))
    open(OPERATIONS_LOG, "a") do f
        println(f, "$(now())\t$repo\t$operation\t$result")
    end
end

# =============================================================================
# CLI Functions
# =============================================================================

"""
List all operations
"""
function list_operations()
    println("\n=== Available Operations ===\n")

    categories = Dict{String, Vector{Pair{String, NamedTuple}}}()
    for (id, op) in OPERATIONS
        cat = op.category
        if !haskey(categories, cat)
            categories[cat] = Pair{String, NamedTuple}[]
        end
        push!(categories[cat], id => op)
    end

    for cat in ["structure", "cleanup", "workflows", "docs", "sync"]
        if haskey(categories, cat)
            println("[$cat]")
            for (id, op) in sort(categories[cat], by=x->x.first)
                println("  $id")
                println("    $(op.name): $(op.description)")
            end
            println()
        end
    end
end

"""
List all repos with metadata
"""
function list_repos()
    println("Scanning repos in $REPOS_DIR...")

    repos = RepoMeta[]
    for name in readdir(REPOS_DIR)
        path = joinpath(REPOS_DIR, name)
        meta = scan_repo(path)
        if meta !== nothing
            push!(repos, meta)
        end
    end

    println("\n=== Repository Summary ===\n")
    println("Total repos: $(length(repos))")

    # Count by type
    types = Dict{String, Int}()
    for r in repos
        types[r.type] = get(types, r.type, 0) + 1
    end
    println("\nBy Type:")
    for (t, c) in sort(collect(types), by=x->-x[2])
        println("  $t: $c")
    end

    # Count with banned languages
    has_ts = count(r -> r.has_typescript, repos)
    has_py = count(r -> r.has_python, repos)
    has_go = count(r -> r.has_go, repos)
    has_rb = count(r -> r.has_ruby, repos)
    has_mk = count(r -> r.has_makefile, repos)
    has_jk = count(r -> r.has_jekyll, repos)

    println("\nBanned Artifacts:")
    println("  TypeScript: $has_ts")
    println("  Python: $has_py")
    println("  Go: $has_go")
    println("  Ruby: $has_rb")
    println("  Makefile: $has_mk")
    println("  Jekyll: $has_jk")

    # Count with missing SCM
    missing = count(r -> !isempty(r.missing_scm), repos)
    println("\nMissing SCM files: $missing repos")

    repos
end

"""
Interactive selection menu
"""
function interactive_select()
    repos = list_repos()

    println("\n=== Selection Filters ===")
    println("1. All repos")
    println("2. By language")
    println("3. By type (ssg, mcp, library, etc.)")
    println("4. Has banned languages")
    println("5. Has Jekyll")
    println("6. Missing SCM files")
    println("7. Specific repos (comma-separated)")
    print("\nSelect filter [1-7]: ")

    choice = readline()
    selected = String[]

    if choice == "1"
        selected = [r.name for r in repos]
    elseif choice == "2"
        print("Enter language (rust, rescript, deno, etc.): ")
        lang = readline()
        selected = [r.name for r in filter_repos(repos, language=lang)]
    elseif choice == "3"
        print("Enter type (ssg, mcp, library, application, spec): ")
        type = readline()
        selected = [r.name for r in filter_repos(repos, type=type)]
    elseif choice == "4"
        selected = [r.name for r in filter_repos(repos, has_banned=true)]
    elseif choice == "5"
        selected = [r.name for r in filter_repos(repos, has_jekyll=true)]
    elseif choice == "6"
        selected = [r.name for r in filter_repos(repos, missing_scm=true)]
    elseif choice == "7"
        print("Enter repo names (comma-separated): ")
        names = readline()
        selected = strip.(split(names, ","))
    end

    println("\nSelected $(length(selected)) repos:")
    for r in selected[1:min(10, length(selected))]
        println("  - $r")
    end
    if length(selected) > 10
        println("  ... and $(length(selected) - 10) more")
    end

    print("\nSave selection? [y/N]: ")
    if lowercase(readline()) == "y"
        save_selection(selected)
    end

    selected
end

# =============================================================================
# Main Entry Point
# =============================================================================

function main()
    args = ARGS

    if isempty(args)
        println("Usage:")
        println("  julia repo-batch-ops.jl --list-repos        # Show all repos")
        println("  julia repo-batch-ops.jl --select            # Interactive selection")
        println("  julia repo-batch-ops.jl --checklist         # Show operations")
        println("  julia repo-batch-ops.jl --run <op>          # Run operation")
        println("  julia repo-batch-ops.jl --dry-run <op>      # Preview operation")
        return
    end

    cmd = args[1]

    if cmd == "--list-repos"
        list_repos()
    elseif cmd == "--select"
        interactive_select()
    elseif cmd == "--checklist"
        list_operations()
    elseif cmd == "--run" && length(args) >= 2
        operation = args[2]
        repos = load_selection()
        if isempty(repos)
            println("No repos selected. Run --select first.")
            return
        end
        run_operation(operation, repos, dry_run=false)
    elseif cmd == "--dry-run" && length(args) >= 2
        operation = args[2]
        repos = load_selection()
        if isempty(repos)
            println("No repos selected. Run --select first.")
            return
        end
        run_operation(operation, repos, dry_run=true)
    else
        println("Unknown command: $cmd")
    end
end

# Run if executed directly
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
