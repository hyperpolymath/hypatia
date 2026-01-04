# SPDX-License-Identifier: MPL-2.0
# SPDX-FileCopyrightText: 2025 Hyperpolymath
#
# Repository Quality Audit Script
# Identifies repos with quality issues:
# - Missing/inaccurate descriptions
# - Missing/insufficient tags
# - Underdeveloped READMEs
# - Generic/standards-focused-only descriptions
#
# Usage:
#   julia audit-repos.jl                    # Full audit
#   julia audit-repos.jl --descriptions     # Description issues only
#   julia audit-repos.jl --tags             # Tag issues only
#   julia audit-repos.jl --readme           # README issues only
#   julia audit-repos.jl --fix              # Generate fix suggestions
#

using Dates
using JSON

# =============================================================================
# Configuration
# =============================================================================

const GITHUB_ORG = "hyperpolymath"
const REPOS_DIR = expanduser("~/repos")
const AUDIT_OUTPUT = expanduser("~/.git-private-farm/audit-results.json")

# Generic/boilerplate description patterns to flag
const GENERIC_PATTERNS = [
    r"^$",                                    # Empty
    r"^null$"i,                               # GitHub null
    r"^No description"i,
    r"^A?\s*(simple|basic)\s*"i,              # "A simple..."
    r"^This (is|repo|repository)"i,           # "This is..."
    r"^TODO"i,
    r"^WIP"i,
    r"^Template"i,
    r"^Placeholder"i,
    r"^Test"i,
    r"^Example"i,
]

# Standards-only description patterns (should focus on project, not just tech)
const STANDARDS_ONLY_PATTERNS = [
    r"^RSR\s*(compliant|standard|implementation)$"i,
    r"^(OpenSSF|SLSA)\s*compliant$"i,
    r"^(Rust|ReScript|Deno|TypeScript)\s*(project|repo|repository)?$"i,
    r"^Built with"i,
    r"^Uses?"i,
    r"^Implements?\s+(the\s+)?[\w-]+\s*standard$"i,
]

# Minimum expected topics/tags for a repo (4-8 content-specific + "rhodium")
const MIN_TAGS = 4
const MAX_TAGS = 8
const IDEAL_MIN = 5

# Minimum README lines for a "developed" repo
const MIN_README_LINES = 10

# =============================================================================
# Audit Result Types
# =============================================================================

@enum IssueType begin
    NO_DESCRIPTION
    GENERIC_DESCRIPTION
    STANDARDS_ONLY_DESCRIPTION
    DESCRIPTION_NAME_MISMATCH
    NO_TAGS
    INSUFFICIENT_TAGS
    MISSING_LANGUAGE_TAG
    NO_README
    UNDERDEVELOPED_README
    README_IS_TEMPLATE
end

struct AuditIssue
    repo::String
    issue_type::IssueType
    severity::Symbol  # :critical, :warning, :notice
    details::String
    suggestion::String
end

# =============================================================================
# GitHub API Functions
# =============================================================================

"""
Get repository metadata from GitHub API
"""
function get_repo_info(repo_name::String)
    try
        output = read(`gh api repos/$GITHUB_ORG/$repo_name`, String)
        JSON.parse(output)
    catch e
        @warn "Failed to get repo info for $repo_name: $e"
        nothing
    end
end

"""
Get repository topics/tags from GitHub API
"""
function get_repo_topics(repo_name::String)
    try
        output = read(`gh api repos/$GITHUB_ORG/$repo_name/topics`, String)
        data = JSON.parse(output)
        get(data, "names", String[])
    catch
        String[]
    end
end

# =============================================================================
# Audit Functions
# =============================================================================

"""
Check if description is missing or empty
"""
function check_no_description(description::Union{String, Nothing})::Bool
    description === nothing || isempty(strip(string(description)))
end

"""
Check if description is generic/boilerplate
"""
function check_generic_description(description::String)::Bool
    desc = strip(description)
    any(p -> occursin(p, desc), GENERIC_PATTERNS)
end

"""
Check if description only mentions standards/technologies
"""
function check_standards_only(description::String)::Bool
    desc = strip(description)
    any(p -> occursin(p, desc), STANDARDS_ONLY_PATTERNS)
end

"""
Check if description matches repo name/purpose
"""
function check_description_mismatch(repo_name::String, description::String)::Tuple{Bool, String}
    desc_lower = lowercase(description)
    name_parts = split(replace(repo_name, "-" => " ", "_" => " "))

    # Check if any meaningful part of repo name is in description
    significant_parts = filter(p -> length(p) > 2 && p ∉ ["the", "and", "for", "mcp", "ssg"], name_parts)

    if isempty(significant_parts)
        return (false, "")
    end

    matches = count(p -> occursin(lowercase(p), desc_lower), significant_parts)

    if matches == 0
        return (true, "Description doesn't reference any key terms from repo name: $(join(significant_parts, ", "))")
    end

    (false, "")
end

"""
Check README quality
"""
function check_readme(repo_path::String)::Vector{AuditIssue}
    issues = AuditIssue[]
    repo_name = basename(repo_path)

    # Check for README existence
    readme_path = ""
    for candidate in ["README.adoc", "README.md", "readme.adoc", "readme.md"]
        p = joinpath(repo_path, candidate)
        if isfile(p)
            readme_path = p
            break
        end
    end

    if isempty(readme_path)
        push!(issues, AuditIssue(
            repo_name,
            NO_README,
            :critical,
            "No README file found",
            "Create README.adoc with project description, installation, and usage"
        ))
        return issues
    end

    # Check README content
    content = read(readme_path, String)
    lines = split(content, '\n')
    non_empty_lines = filter(l -> !isempty(strip(l)), lines)

    if length(non_empty_lines) < MIN_README_LINES
        push!(issues, AuditIssue(
            repo_name,
            UNDERDEVELOPED_README,
            :warning,
            "README has only $(length(non_empty_lines)) non-empty lines (minimum: $MIN_README_LINES)",
            "Expand README with: overview, installation, usage examples, contributing"
        ))
    end

    # Check for template-only content
    template_markers = [
        r"^= Project Name$"m,
        r"^\# Project Name$"m,
        r"^TODO:?\s*"im,
        r"^\[Insert"im,
        r"^Replace this"im,
        r"^\{\{.*\}\}"m,
    ]

    if any(m -> occursin(m, content), template_markers)
        push!(issues, AuditIssue(
            repo_name,
            README_IS_TEMPLATE,
            :critical,
            "README appears to be an unfilled template",
            "Replace template placeholders with actual project content"
        ))
    end

    issues
end

"""
Check tags/topics - Policy:
- "rhodium" tag only (not rhodium-standard, rsr, etc.)
- 4-8 OTHER content-specific tags
- Language tags only when language is focus (FFI/ABI, SSGs, playgrounds)
"""
function check_tags(repo_name::String, topics::Vector{String}, detected_languages::Vector{String})::Vector{AuditIssue}
    issues = AuditIssue[]

    # Filter out "rhodium" for counting content tags
    content_tags = filter(t -> lowercase(t) ∉ ["rhodium", "rhodium-standard", "rsr", "rsr-compliant"], topics)
    has_rhodium = any(t -> lowercase(t) == "rhodium", topics)

    # Check for redundant compliance tags
    bad_compliance_tags = filter(t -> lowercase(t) in ["rhodium-standard", "rsr", "rsr-compliant", "openssf-scorecard"], topics)
    if !isempty(bad_compliance_tags)
        push!(issues, AuditIssue(
            repo_name,
            INSUFFICIENT_TAGS,  # Reusing type
            :notice,
            "Redundant compliance tags: $(join(bad_compliance_tags, ", "))",
            "Use only 'rhodium' tag for RSR compliance (remove: $(join(bad_compliance_tags, ", ")))"
        ))
    end

    if isempty(topics)
        push!(issues, AuditIssue(
            repo_name,
            NO_TAGS,
            :warning,
            "No topics/tags set for repository",
            "Add 'rhodium' + 4-8 content tags: $(join(suggest_tags(repo_name, detected_languages), ", "))"
        ))
        return issues
    end

    if !has_rhodium
        push!(issues, AuditIssue(
            repo_name,
            INSUFFICIENT_TAGS,
            :notice,
            "Missing 'rhodium' compliance tag",
            "Add 'rhodium' tag for RSR compliance"
        ))
    end

    if length(content_tags) < MIN_TAGS
        push!(issues, AuditIssue(
            repo_name,
            INSUFFICIENT_TAGS,
            :notice,
            "Only $(length(content_tags)) content tags (recommended: $MIN_TAGS-$MAX_TAGS)",
            "Add content-specific tags: $(join(suggest_tags(repo_name, detected_languages), ", "))"
        ))
    end

    # Language tags only when appropriate
    is_language_focused = contains(repo_name, "playground") ||
                          contains(repo_name, "-ffi") ||
                          contains(repo_name, "-abi") ||
                          endswith(repo_name, "-ssg") ||
                          contains(repo_name, "lang") ||
                          contains(repo_name, "compiler")

    lang_topics = ["rust", "rescript", "deno", "gleam", "ocaml", "haskell", "julia", "nickel", "zig"]
    has_lang_tag = any(t -> lowercase(t) in lang_topics, topics)

    if has_lang_tag && !is_language_focused
        push!(issues, AuditIssue(
            repo_name,
            MISSING_LANGUAGE_TAG,  # Reusing type for opposite case
            :notice,
            "Has language tags but repo isn't language-focused",
            "Consider removing language tags unless project is FFI/ABI/SSG/playground"
        ))
    end

    issues
end

"""
Suggest appropriate tags for a repo
Policy: focus on CONTENT, not implementation or compliance
"""
function suggest_tags(repo_name::String, detected_languages::Vector{String})::Vector{String}
    suggestions = ["rhodium"]  # Always include rhodium

    # Check if language-focused (only then add language tags)
    is_language_focused = contains(repo_name, "playground") ||
                          contains(repo_name, "-ffi") ||
                          contains(repo_name, "-abi") ||
                          endswith(repo_name, "-ssg") ||
                          contains(repo_name, "lang") ||
                          contains(repo_name, "compiler")

    if is_language_focused
        append!(suggestions, detected_languages)
    end

    # Add domain-based tags from repo name
    name_parts = lowercase.(split(replace(repo_name, "-" => " ", "_" => " ")))

    # Map common terms to tags
    domain_tags = Dict(
        "ssg" => ["static-site-generator", "web"],
        "mcp" => ["model-context-protocol", "ai-tooling"],
        "bot" => ["automation", "bot"],
        "cli" => ["command-line", "cli"],
        "api" => ["api", "backend"],
        "db" => ["database"],
        "auth" => ["authentication", "security"],
        "crypto" => ["cryptography", "security"],
        "config" => ["configuration"],
        "i18n" => ["internationalization", "localization"],
        "playground" => ["learning", "experimental"],
        "spec" => ["specification", "documentation"],
        "standard" => ["specification", "standards"],
        "template" => ["template", "scaffolding"],
        "wasm" => ["webassembly"],
        "web" => ["web"],
        "mobile" => ["mobile"],
        "desktop" => ["desktop"],
        "cloud" => ["cloud", "infrastructure"],
        "k8s" => ["kubernetes", "containers"],
        "container" => ["containers", "docker"],
        "network" => ["networking"],
        "security" => ["security"],
        "verify" => ["verification", "formal-methods"],
        "proof" => ["formal-methods", "verification"],
        "llm" => ["ai", "llm"],
        "ml" => ["machine-learning", "ai"],
        "git" => ["git", "version-control"],
        "zotero" => ["zotero", "research", "bibliography"],
    )

    for part in name_parts
        if haskey(domain_tags, part)
            append!(suggestions, domain_tags[part])
        end
    end

    unique(suggestions)
end

"""
Detect languages in a repository
"""
function detect_languages_local(repo_path::String)::Vector{String}
    languages = String[]

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
    )

    for (lang, files) in indicators
        for file in files
            if contains(file, "*")
                pattern = replace(file, "*" => "")
                if any(f -> endswith(f, pattern), readdir(repo_path))
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
Run full audit on a single repo
"""
function audit_repo(repo_name::String)::Vector{AuditIssue}
    issues = AuditIssue[]
    repo_path = joinpath(REPOS_DIR, repo_name)

    # Get GitHub metadata
    info = get_repo_info(repo_name)
    topics = get_repo_topics(repo_name)
    detected_langs = detect_languages_local(repo_path)

    if info !== nothing
        description = get(info, "description", nothing)

        # Check description issues
        if check_no_description(description)
            push!(issues, AuditIssue(
                repo_name,
                NO_DESCRIPTION,
                :critical,
                "Repository has no description",
                "Add a description that explains the project's purpose"
            ))
        elseif description !== nothing
            desc = string(description)

            if check_generic_description(desc)
                push!(issues, AuditIssue(
                    repo_name,
                    GENERIC_DESCRIPTION,
                    :warning,
                    "Description is generic: \"$desc\"",
                    "Replace with specific project purpose and functionality"
                ))
            end

            if check_standards_only(desc)
                push!(issues, AuditIssue(
                    repo_name,
                    STANDARDS_ONLY_DESCRIPTION,
                    :warning,
                    "Description only mentions standards/technology: \"$desc\"",
                    "Focus on WHAT the project does, not just how it's built"
                ))
            end

            is_mismatch, mismatch_details = check_description_mismatch(repo_name, desc)
            if is_mismatch
                push!(issues, AuditIssue(
                    repo_name,
                    DESCRIPTION_NAME_MISMATCH,
                    :notice,
                    mismatch_details,
                    "Ensure description reflects the project name's purpose"
                ))
            end
        end
    end

    # Check tags
    append!(issues, check_tags(repo_name, topics, detected_langs))

    # Check README (local)
    if isdir(repo_path)
        append!(issues, check_readme(repo_path))
    end

    issues
end

"""
Run audit on all repos
"""
function audit_all_repos(; filter_type::Union{Symbol, Nothing}=nothing)
    all_issues = AuditIssue[]

    # Get list of repos
    repos = String[]

    # From local repos dir
    for name in readdir(REPOS_DIR)
        path = joinpath(REPOS_DIR, name)
        if isdir(joinpath(path, ".git"))
            push!(repos, name)
        end
    end

    println("Auditing $(length(repos)) repositories...")

    for (i, repo) in enumerate(repos)
        print("\r[$i/$(length(repos))] Auditing: $repo                    ")
        issues = audit_repo(repo)

        if filter_type !== nothing
            # Filter by issue type category
            if filter_type == :descriptions
                issues = filter(i -> i.issue_type in [NO_DESCRIPTION, GENERIC_DESCRIPTION, STANDARDS_ONLY_DESCRIPTION, DESCRIPTION_NAME_MISMATCH], issues)
            elseif filter_type == :tags
                issues = filter(i -> i.issue_type in [NO_TAGS, INSUFFICIENT_TAGS, MISSING_LANGUAGE_TAG], issues)
            elseif filter_type == :readme
                issues = filter(i -> i.issue_type in [NO_README, UNDERDEVELOPED_README, README_IS_TEMPLATE], issues)
            end
        end

        append!(all_issues, issues)

        # Rate limit API calls
        sleep(0.1)
    end
    println()

    all_issues
end

"""
Generate audit report
"""
function generate_report(issues::Vector{AuditIssue})
    println("\n" * "="^70)
    println("                    REPOSITORY AUDIT REPORT")
    println("="^70)
    println("Generated: $(now())")
    println("Total issues found: $(length(issues))")
    println()

    # Group by severity
    critical = filter(i -> i.severity == :critical, issues)
    warning = filter(i -> i.severity == :warning, issues)
    notice = filter(i -> i.severity == :notice, issues)

    println("By Severity:")
    println("  🔴 Critical: $(length(critical))")
    println("  🟡 Warning:  $(length(warning))")
    println("  🔵 Notice:   $(length(notice))")
    println()

    # Group by type
    println("By Issue Type:")
    type_counts = Dict{IssueType, Int}()
    for issue in issues
        type_counts[issue.issue_type] = get(type_counts, issue.issue_type, 0) + 1
    end
    for (t, c) in sort(collect(type_counts), by=x->-x[2])
        println("  $(string(t)): $c")
    end
    println()

    # List critical issues
    if !isempty(critical)
        println("-"^70)
        println("CRITICAL ISSUES (Immediate Action Required)")
        println("-"^70)
        for issue in critical
            println("• $(issue.repo)")
            println("  Issue: $(issue.details)")
            println("  Fix: $(issue.suggestion)")
            println()
        end
    end

    # List warnings (top 20)
    if !isempty(warning)
        println("-"^70)
        println("WARNING ISSUES (Top 20)")
        println("-"^70)
        for issue in warning[1:min(20, length(warning))]
            println("• $(issue.repo): $(issue.details)")
        end
        if length(warning) > 20
            println("  ... and $(length(warning) - 20) more")
        end
        println()
    end

    # Save full results to JSON
    save_results(issues)
end

"""
Save results to JSON
"""
function save_results(issues::Vector{AuditIssue})
    mkpath(dirname(AUDIT_OUTPUT))

    results = [
        Dict(
            "repo" => i.repo,
            "issue_type" => string(i.issue_type),
            "severity" => string(i.severity),
            "details" => i.details,
            "suggestion" => i.suggestion
        )
        for i in issues
    ]

    open(AUDIT_OUTPUT, "w") do f
        JSON.print(f, Dict(
            "timestamp" => string(now()),
            "total_issues" => length(issues),
            "issues" => results
        ), 2)
    end

    println("Full results saved to: $AUDIT_OUTPUT")
end

# =============================================================================
# Main Entry Point
# =============================================================================

function main()
    args = ARGS

    if isempty(args)
        issues = audit_all_repos()
        generate_report(issues)
    elseif args[1] == "--descriptions"
        issues = audit_all_repos(filter_type=:descriptions)
        generate_report(issues)
    elseif args[1] == "--tags"
        issues = audit_all_repos(filter_type=:tags)
        generate_report(issues)
    elseif args[1] == "--readme"
        issues = audit_all_repos(filter_type=:readme)
        generate_report(issues)
    elseif args[1] == "--fix"
        issues = audit_all_repos()
        println("\n=== FIX SUGGESTIONS ===\n")
        for issue in filter(i -> i.severity == :critical, issues)
            println("gh repo edit $GITHUB_ORG/$(issue.repo) --description \"<TODO: describe $(issue.repo)>\"")
        end
    else
        println("Usage:")
        println("  julia audit-repos.jl              # Full audit")
        println("  julia audit-repos.jl --descriptions")
        println("  julia audit-repos.jl --tags")
        println("  julia audit-repos.jl --readme")
        println("  julia audit-repos.jl --fix        # Generate fix commands")
    end
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
