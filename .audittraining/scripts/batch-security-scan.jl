#!/usr/bin/env julia
# SPDX-License-Identifier: PMPL-1.0-or-later
# Batch security scanner for hyperpolymath org
# Generates training data for cicd-hyper-a and echidna learning

using JSON3
using Dates
using HTTP

const ORG = "hyperpolymath"
const OUTPUT_DIR = joinpath(@__DIR__, "..", "security-errors")
const ECHIDNA_RULES_DIR = expanduser("~/repos/echidna/rules")

# Alert categories for echidna learning
const ALERT_CATEGORIES = Dict(
    "workflow-security" => ["TokenPermissionsID", "PinnedDependenciesID", "missing-workflow-permissions"],
    "code-security" => ["hard-coded-cryptographic-value", "remote-property-injection", "sql-injection"],
    "code-quality" => ["unused-local-variable", "syntax-error", "unused-import"],
    "dependency-vuln" => ["VulnerabilitiesID", "unmaintained"],
    "process-hygiene" => ["SecurityPolicyID", "MaintainedID", "CodeReviewID", "BranchProtectionID", "CIIBestPracticesID"],
    "missing-tests" => ["CITestsID", "FuzzingID", "SASTID"]
)

# Auto-fixable alerts
const AUTO_FIXABLE = Set([
    "TokenPermissionsID", "PinnedDependenciesID", "missing-workflow-permissions",
    "SecurityPolicyID", "BranchProtectionID", "unused-local-variable"
])

struct Alert
    repo::String
    rule_id::String
    description::String
    file::Union{String, Nothing}
    line::Union{Int, Nothing}
    state::String
    category::String
    auto_fixable::Bool
end

function categorize_alert(rule_id::String)::String
    for (cat, rules) in ALERT_CATEGORIES
        if any(r -> occursin(r, rule_id), rules)
            return cat
        end
    end
    return "unknown"
end

function fetch_alerts(repo::String)::Vector{Alert}
    alerts = Alert[]

    try
        cmd = `gh api repos/$ORG/$repo/code-scanning/alerts --jq '[.[] | {rule: .rule.id, desc: .rule.description, file: .most_recent_instance.location.path, line: .most_recent_instance.location.start_line, state: .state}]'`
        output = read(cmd, String)

        if !isempty(output)
            data = JSON3.read(output)
            for item in data
                rule_id = string(item.rule)
                push!(alerts, Alert(
                    repo,
                    rule_id,
                    string(get(item, :desc, "")),
                    get(item, :file, nothing),
                    get(item, :line, nothing),
                    string(get(item, :state, "open")),
                    categorize_alert(rule_id),
                    rule_id in AUTO_FIXABLE
                ))
            end
        end
    catch e
        @warn "Failed to fetch alerts for $repo: $e"
    end

    return alerts
end

function generate_repo_training_data(repo::String, alerts::Vector{Alert})
    open_alerts = filter(a -> a.state == "open", alerts)
    isempty(open_alerts) && return

    # Group by category
    by_category = Dict{String, Vector{Alert}}()
    for alert in open_alerts
        cat = alert.category
        if !haskey(by_category, cat)
            by_category[cat] = Alert[]
        end
        push!(by_category[cat], alert)
    end

    # Generate markdown
    md = """
    # $repo - Security Audit

    ## Repository Info
    - **URL:** https://github.com/$ORG/$repo
    - **Audit Date:** $(Dates.format(now(), "yyyy-mm-dd"))
    - **Total Open Alerts:** $(length(open_alerts))

    ---

    ## Issues by Category

    """

    for (cat, cat_alerts) in sort(collect(by_category), by=x->length(x[2]), rev=true)
        md *= "### $(titlecase(replace(cat, "-" => " "))) ($(length(cat_alerts)))\n\n"
        md *= "| Rule | File | Auto-Fix |\n"
        md *= "|------|------|----------|\n"

        for alert in cat_alerts
            file = isnothing(alert.file) ? "N/A" : alert.file
            auto = alert.auto_fixable ? "✓" : "✗"
            md *= "| $(alert.rule_id) | $file | $auto |\n"
        end
        md *= "\n"
    end

    # Write file
    filepath = joinpath(OUTPUT_DIR, "$repo.md")
    write(filepath, md)
    println("Generated: $filepath")
end

function generate_echidna_rules(all_alerts::Vector{Alert})
    # Group by rule_id for pattern learning
    by_rule = Dict{String, Int}()
    for alert in all_alerts
        rule = alert.rule_id
        by_rule[rule] = get(by_rule, rule, 0) + 1
    end

    # Generate Logtalk-style rules
    rules = """
    %% SPDX-License-Identifier: PMPL-1.0-or-later
    %% Auto-generated security rules from $(length(all_alerts)) alerts across hyperpolymath org
    %% Generated: $(Dates.format(now(), "yyyy-mm-dd HH:MM:SS"))

    :- object(security_rules).

    :- public([
        check_workflow/2,
        check_code/2,
        check_dependencies/2
    ]).

    """

    for (rule_id, count) in sort(collect(by_rule), by=x->x[2], rev=true)
        category = categorize_alert(rule_id)
        auto_fix = rule_id in AUTO_FIXABLE

        rules *= """
        %% Rule: $rule_id (seen $count times)
        %% Category: $category
        %% Auto-fixable: $auto_fix
        security_rule('$rule_id', $category, $auto_fix, $count).

        """
    end

    rules *= """
    :- end_object.
    """

    # Write to echidna rules directory
    if isdir(ECHIDNA_RULES_DIR)
        filepath = joinpath(ECHIDNA_RULES_DIR, "learned_security_rules.lgt")
        write(filepath, rules)
        println("Generated echidna rules: $filepath")
    else
        # Fallback to cicd-hyper-a
        filepath = joinpath(OUTPUT_DIR, "..", "echidna-rules.lgt")
        write(filepath, rules)
        println("Generated echidna rules: $filepath")
    end
end

function generate_summary(all_alerts::Vector{Alert}, repos_processed::Int)
    open_alerts = filter(a -> a.state == "open", all_alerts)

    summary = """
    # Security Audit Summary - $(Dates.format(now(), "yyyy-mm-dd"))

    ## Statistics
    - **Repos Scanned:** $repos_processed
    - **Total Alerts:** $(length(all_alerts))
    - **Open Alerts:** $(length(open_alerts))
    - **Auto-Fixable:** $(count(a -> a.auto_fixable, open_alerts))

    ## Alerts by Category

    | Category | Count | Auto-Fixable |
    |----------|-------|--------------|
    """

    by_category = Dict{String, Tuple{Int, Int}}()
    for alert in open_alerts
        cat = alert.category
        curr = get(by_category, cat, (0, 0))
        by_category[cat] = (curr[1] + 1, curr[2] + (alert.auto_fixable ? 1 : 0))
    end

    for (cat, (total, fixable)) in sort(collect(by_category), by=x->x[2][1], rev=true)
        summary *= "| $cat | $total | $fixable |\n"
    end

    summary *= """

    ## Top 20 Repos by Alert Count

    | Repo | Alerts |
    |------|--------|
    """

    by_repo = Dict{String, Int}()
    for alert in open_alerts
        by_repo[alert.repo] = get(by_repo, alert.repo, 0) + 1
    end

    for (repo, count) in Iterators.take(sort(collect(by_repo), by=x->x[2], rev=true), 20)
        summary *= "| $repo | $count |\n"
    end

    filepath = joinpath(OUTPUT_DIR, "SUMMARY.md")
    write(filepath, summary)
    println("Generated: $filepath")
end

function main()
    println("🔍 Scanning hyperpolymath org for security alerts...")

    # Get all repos
    repos_output = read(`gh repo list $ORG --limit 300 --json name --jq '.[].name'`, String)
    repos = filter(!isempty, split(repos_output, "\n"))

    println("Found $(length(repos)) repos")

    all_alerts = Alert[]
    repos_with_alerts = 0

    for (i, repo) in enumerate(repos)
        print("\r[$i/$(length(repos))] Scanning $repo...                    ")
        alerts = fetch_alerts(repo)

        if !isempty(alerts)
            repos_with_alerts += 1
            append!(all_alerts, alerts)

            # Generate per-repo training data
            generate_repo_training_data(repo, alerts)
        end
    end

    println("\n\n📊 Results:")
    println("  Repos with alerts: $repos_with_alerts")
    println("  Total alerts: $(length(all_alerts))")

    # Generate summary
    generate_summary(all_alerts, length(repos))

    # Generate echidna learning rules
    generate_echidna_rules(all_alerts)

    println("\n✅ Complete! Training data generated in $OUTPUT_DIR")
end

# Run if called directly
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
