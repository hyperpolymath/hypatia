# SPDX-License-Identifier: PMPL-1.0-or-later
defmodule Mix.Tasks.Hypatia.BatchSecurityScan do
  @moduledoc """
  Scan the hyperpolymath org for code-scanning alerts and generate
  training data for Hypatia + echidnabot.

  Ported from `.audittraining/scripts/batch-security-scan.jl`.

  ## Usage

      mix hypatia.batch_security_scan

  Outputs:
    * `.audittraining/security-errors/<repo>.md` — per-repo audit markdown
    * `.audittraining/security-errors/SUMMARY.md` — org-wide summary
    * `~/repos/echidna/rules/learned_security_rules.lgt` (if that dir
      exists) or `.audittraining/echidna-rules.lgt` — Logtalk-format
      rules for echidnabot. NB: Hypatia retired its own Logtalk engine;
      this output is purely for consumption by echidnabot in a separate
      repo.
  """

  use Mix.Task

  @shortdoc "Scan org for code-scanning alerts + generate training data"

  @org "hyperpolymath"
  @output_dir ".audittraining/security-errors"
  @echidna_rules_dir "~/repos/echidna/rules"

  @alert_categories %{
    "workflow-security" => ~w(TokenPermissionsID PinnedDependenciesID missing-workflow-permissions),
    "code-security" => ~w(hard-coded-cryptographic-value remote-property-injection sql-injection),
    "code-quality" => ~w(unused-local-variable syntax-error unused-import),
    "dependency-vuln" => ~w(VulnerabilitiesID unmaintained),
    "process-hygiene" =>
      ~w(SecurityPolicyID MaintainedID CodeReviewID BranchProtectionID CIIBestPracticesID),
    "missing-tests" => ~w(CITestsID FuzzingID SASTID)
  }

  @auto_fixable MapSet.new(
                  ~w(TokenPermissionsID PinnedDependenciesID missing-workflow-permissions
                     SecurityPolicyID BranchProtectionID unused-local-variable)
                )

  @impl Mix.Task
  def run(_args) do
    Mix.shell().info("🔍 Scanning #{@org} org for security alerts...")

    {repos_out, 0} =
      System.cmd("gh", ["repo", "list", @org, "--limit", "300", "--json", "name", "--jq", ".[].name"])

    repos = String.split(repos_out, "\n", trim: true)
    Mix.shell().info("Found #{length(repos)} repos")

    File.mkdir_p!(@output_dir)

    {all_alerts, repos_with_alerts} =
      repos
      |> Enum.with_index(1)
      |> Enum.reduce({[], 0}, fn {repo, i}, {acc, count} ->
        IO.write("\r[#{i}/#{length(repos)}] Scanning #{repo}...                    ")
        alerts = fetch_alerts(repo)

        case alerts do
          [] -> {acc, count}
          _ ->
            generate_repo_training_data(repo, alerts)
            {acc ++ alerts, count + 1}
        end
      end)

    Mix.shell().info("\n\n📊 Results:")
    Mix.shell().info("  Repos with alerts: #{repos_with_alerts}")
    Mix.shell().info("  Total alerts: #{length(all_alerts)}")

    generate_summary(all_alerts, length(repos))
    generate_echidna_rules(all_alerts)

    Mix.shell().info("\n✅ Complete! Training data generated in #{@output_dir}")
  end

  # ---------------------------------------------------------------------

  defp fetch_alerts(repo) do
    cmd_args = [
      "api",
      "repos/#{@org}/#{repo}/code-scanning/alerts",
      "--jq",
      "[.[] | {rule: .rule.id, desc: .rule.description, file: .most_recent_instance.location.path, line: .most_recent_instance.location.start_line, state: .state}]"
    ]

    case System.cmd("gh", cmd_args, stderr_to_stdout: true) do
      {out, 0} when out != "" ->
        case Jason.decode(out) do
          {:ok, list} when is_list(list) ->
            Enum.map(list, fn item ->
              rule_id = Map.get(item, "rule", "")

              %{
                repo: repo,
                rule_id: rule_id,
                description: Map.get(item, "desc", ""),
                file: Map.get(item, "file"),
                line: Map.get(item, "line"),
                state: Map.get(item, "state", "open"),
                category: categorize_alert(rule_id),
                auto_fixable: MapSet.member?(@auto_fixable, rule_id)
              }
            end)

          _ ->
            []
        end

      _ ->
        []
    end
  end

  defp categorize_alert(rule_id) do
    Enum.find_value(@alert_categories, "unknown", fn {cat, rules} ->
      if Enum.any?(rules, &String.contains?(rule_id, &1)), do: cat
    end)
  end

  defp generate_repo_training_data(repo, alerts) do
    open_alerts = Enum.filter(alerts, &(&1.state == "open"))
    if open_alerts == [], do: :ok, else: do_generate_repo_md(repo, open_alerts)
  end

  defp do_generate_repo_md(repo, open_alerts) do
    by_category = Enum.group_by(open_alerts, & &1.category)

    header = """
    # #{repo} - Security Audit

    ## Repository Info
    - **URL:** https://github.com/#{@org}/#{repo}
    - **Audit Date:** #{Date.utc_today()}
    - **Total Open Alerts:** #{length(open_alerts)}

    ---

    ## Issues by Category

    """

    sections =
      by_category
      |> Enum.sort_by(fn {_cat, a} -> -length(a) end)
      |> Enum.map_join("\n", fn {cat, cat_alerts} ->
        title =
          cat
          |> String.replace("-", " ")
          |> String.split(" ")
          |> Enum.map_join(" ", &String.capitalize/1)

        rows =
          Enum.map_join(cat_alerts, "\n", fn a ->
            file = a.file || "N/A"
            auto = if a.auto_fixable, do: "✓", else: "✗"
            "| #{a.rule_id} | #{file} | #{auto} |"
          end)

        """
        ### #{title} (#{length(cat_alerts)})

        | Rule | File | Auto-Fix |
        |------|------|----------|
        #{rows}
        """
      end)

    filepath = Path.join(@output_dir, "#{repo}.md")
    File.write!(filepath, header <> sections)
    Mix.shell().info("\nGenerated: #{filepath}")
  end

  defp generate_summary(all_alerts, repos_processed) do
    open_alerts = Enum.filter(all_alerts, &(&1.state == "open"))

    by_category =
      Enum.reduce(open_alerts, %{}, fn a, acc ->
        {total, fixable} = Map.get(acc, a.category, {0, 0})
        Map.put(acc, a.category, {total + 1, fixable + if(a.auto_fixable, do: 1, else: 0)})
      end)

    by_repo =
      Enum.reduce(open_alerts, %{}, fn a, acc ->
        Map.update(acc, a.repo, 1, &(&1 + 1))
      end)

    cat_rows =
      by_category
      |> Enum.sort_by(fn {_c, {total, _f}} -> -total end)
      |> Enum.map_join("\n", fn {cat, {total, fixable}} -> "| #{cat} | #{total} | #{fixable} |" end)

    repo_rows =
      by_repo
      |> Enum.sort_by(fn {_r, c} -> -c end)
      |> Enum.take(20)
      |> Enum.map_join("\n", fn {repo, count} -> "| #{repo} | #{count} |" end)

    fixable_count = Enum.count(open_alerts, & &1.auto_fixable)

    summary = """
    # Security Audit Summary - #{Date.utc_today()}

    ## Statistics
    - **Repos Scanned:** #{repos_processed}
    - **Total Alerts:** #{length(all_alerts)}
    - **Open Alerts:** #{length(open_alerts)}
    - **Auto-Fixable:** #{fixable_count}

    ## Alerts by Category

    | Category | Count | Auto-Fixable |
    |----------|-------|--------------|
    #{cat_rows}

    ## Top 20 Repos by Alert Count

    | Repo | Alerts |
    |------|--------|
    #{repo_rows}
    """

    filepath = Path.join(@output_dir, "SUMMARY.md")
    File.write!(filepath, summary)
    Mix.shell().info("Generated: #{filepath}")
  end

  defp generate_echidna_rules(all_alerts) do
    by_rule =
      Enum.reduce(all_alerts, %{}, fn a, acc -> Map.update(acc, a.rule_id, 1, &(&1 + 1)) end)

    now = DateTime.utc_now() |> DateTime.truncate(:second) |> DateTime.to_iso8601()

    header = """
    %% SPDX-License-Identifier: PMPL-1.0-or-later
    %% Auto-generated security rules from #{length(all_alerts)} alerts across hyperpolymath org
    %% Generated: #{now}

    :- object(security_rules).

    :- public([
        check_workflow/2,
        check_code/2,
        check_dependencies/2
    ]).

    """

    body =
      by_rule
      |> Enum.sort_by(fn {_r, c} -> -c end)
      |> Enum.map_join("\n", fn {rule_id, count} ->
        category = categorize_alert(rule_id)
        auto_fix = MapSet.member?(@auto_fixable, rule_id)

        """
        %% Rule: #{rule_id} (seen #{count} times)
        %% Category: #{category}
        %% Auto-fixable: #{auto_fix}
        security_rule('#{rule_id}', #{category}, #{auto_fix}, #{count}).
        """
      end)

    footer = "\n:- end_object.\n"

    rules = header <> body <> footer
    echidna_dir = Path.expand(@echidna_rules_dir)

    filepath =
      if File.dir?(echidna_dir) do
        Path.join(echidna_dir, "learned_security_rules.lgt")
      else
        Path.join(".audittraining", "echidna-rules.lgt")
      end

    File.write!(filepath, rules)
    Mix.shell().info("Generated echidna rules: #{filepath}")
  end
end
