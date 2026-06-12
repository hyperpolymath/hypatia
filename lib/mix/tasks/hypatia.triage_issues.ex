# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Mix.Tasks.Hypatia.TriageIssues do
  @moduledoc """
  Cross-repo triage of Hypatia-generated GitHub issues.

  Bulk-closes issues that are KNOWN false positives from before
  PR #314's noise-reduction batch landed. Identifies issues by:

    * Label match — issues created by sustainabot carry one of:
      `hypatia`, `hypatia-security-alert`, `hypatia-advisory`,
      `automated-by-hypatia`.

    * Title / body pattern — the issue text references a specific
      rule that PR #314 either suppressed (workflow_audit retired
      list, migration_rules on soundness fixtures, honest_completion
      namespaced state files) or downgraded below the medium+ Phase 2
      submission floor (low-severity unwrap/expect in cli/bin paths).

  ## Usage

      mix hypatia.triage_issues                          # dry-run, hyperpolymath org
      mix hypatia.triage_issues --owner my-org           # different org
      mix hypatia.triage_issues --apply                  # actually close
      mix hypatia.triage_issues --apply --confirm        # skip preview
      mix hypatia.triage_issues --classes workflow_audit # one class only
      mix hypatia.triage_issues --format json            # JSONL audit log

  ## Auth

  Reads HYPATIA_DISPATCH_PAT (the same token configured for
  hypatia-remediation-sweep.yml). Needs `Issues: write` on every
  consuming repo. If you only have read access, dry-run still works
  — it just won't close anything.

  ## Exit codes

  0 — no false-positives found OR --apply succeeded
  1 — bad arguments / token missing / API error during apply
  2 — dry-run found issues to close (use as a signal in CI)

  The non-zero dry-run exit lets you wire this into a periodic CI
  check that surfaces fresh false-positive bursts.

  ## What this is NOT

  This task does not auto-close ALL hypatia-labelled issues — only
  those matching the documented false-positive classes. A real
  finding (e.g. a leaked secret, a missing SPDX header on a non-
  fixture file) is NOT in the close set even if it carries a
  `hypatia` label.
  """

  use Mix.Task

  require Logger

  @shortdoc "Bulk-close estate Hypatia issues that PR #314 invalidated"

  @switches [
    owner: :string,
    apply: :boolean,
    confirm: :boolean,
    classes: :string,
    format: :string,
    labels: :string,
    limit: :integer
  ]

  # Workflows removed from @standard_workflows in PR #314. Any issue
  # whose title or body references "missing workflow: <one of these>"
  # is a false positive on every repo that doesn't need that workflow.
  @retired_workflows ~w(
    quality.yml
    guix-nix-policy.yml
    npm-bun-blocker.yml
    ts-blocker.yml
    security-policy.yml
    rsr-antipattern.yml
    wellknown-enforcement.yml
    workflow-linter.yml
    instant-sync.yml
    jekyll.yml
    jekyll-gh-pages.yml
    scorecard-enforcer.yml
  )

  @close_classes [
    :workflow_audit,
    :code_safety_low,
    :migration_rules_fixture,
    :honest_completion_namespaced
  ]

  @impl Mix.Task
  def run(argv) do
    {opts, _, _} = OptionParser.parse(argv, switches: @switches)

    owner = Keyword.get(opts, :owner, "hyperpolymath")
    apply? = Keyword.get(opts, :apply, false)
    confirm? = Keyword.get(opts, :confirm, false)
    format = Keyword.get(opts, :format, "text")

    labels =
      opts
      |> Keyword.get(
        :labels,
        "hypatia,hypatia-security-alert,hypatia-advisory,automated-by-hypatia"
      )
      |> String.split(",", trim: true)

    classes = parse_classes(Keyword.get(opts, :classes))
    limit = Keyword.get(opts, :limit, 1000)

    token = System.get_env("HYPATIA_DISPATCH_PAT")

    if token in [nil, ""] do
      Mix.shell().error(
        "HYPATIA_DISPATCH_PAT not set. Dry-run still works against public repos; " <>
          "private-repo triage and --apply both require the token."
      )
    end

    candidates = find_candidates(owner, labels, classes, token, limit)

    case format do
      "json" -> emit_json(candidates, owner, apply?)
      _ -> emit_text(candidates, owner, apply?)
    end

    if apply? do
      apply_closes(candidates, token, confirm?)
    else
      if length(candidates) > 0, do: exit({:shutdown, 2}), else: :ok
    end
  end

  defp parse_classes(nil), do: @close_classes

  defp parse_classes(csv) do
    # Resolve against the known-class table instead of atomizing CLI
    # input: dynamic atom creation from arbitrary input leaks atoms (PA013).
    allowed = Map.new(@close_classes, fn c -> {Atom.to_string(c), c} end)

    csv
    |> String.split(",", trim: true)
    |> Enum.flat_map(fn s ->
      case Map.fetch(allowed, s) do
        {:ok, atom} -> [atom]
        :error -> []
      end
    end)
  end

  # ─── Discovery ─────────────────────────────────────────────────────────

  defp find_candidates(owner, labels, classes, token, limit) do
    # Use the GitHub search API rather than enumerating repos —
    # `is:issue is:open org:<owner> label:<x>` returns every match
    # in one query (paginated). Much cheaper than per-repo iteration.
    label_query = labels |> Enum.map(&"label:\"#{&1}\"") |> Enum.join(" ")
    query = "is:issue is:open org:#{owner} #{label_query}"

    fetch_search_results(query, token, limit)
    |> Enum.flat_map(fn issue ->
      case classify(issue, classes) do
        {:close, class, reason} ->
          [%{issue: issue, class: class, reason: reason}]

        :keep ->
          []
      end
    end)
  end

  defp fetch_search_results(query, token, limit) do
    encoded = URI.encode_www_form(query)

    per_page = 100
    pages = ceil(limit / per_page)

    Enum.flat_map(1..pages, fn page ->
      url =
        "https://api.github.com/search/issues?q=#{encoded}" <>
          "&per_page=#{per_page}&page=#{page}"

      headers =
        [
          "-H",
          "Accept: application/vnd.github+json",
          "-H",
          "X-GitHub-Api-Version: 2022-11-28"
        ] ++
          if(token, do: ["-H", "Authorization: Bearer #{token}"], else: [])

      args = ["-sS", "--max-time", "15"] ++ headers ++ [url]

      case System.cmd("curl", args, stderr_to_stdout: true) do
        {body, 0} ->
          case Jason.decode(body) do
            {:ok, %{"items" => items}} -> items
            _ -> []
          end

        _ ->
          []
      end
    end)
    |> Enum.take(limit)
  end

  # ─── Classification ────────────────────────────────────────────────────

  defp classify(issue, classes) do
    title = Map.get(issue, "title", "")
    body = Map.get(issue, "body", "") || ""
    text = (title <> "\n" <> body) |> String.downcase()

    cond do
      :workflow_audit in classes and matches_retired_workflow?(text) ->
        wf = Enum.find(@retired_workflows, fn w -> String.contains?(text, w) end)

        {:close, :workflow_audit,
         "PR #314: '#{wf}' was retired by governance.yml consolidation; " <>
           "scanner no longer expects it."}

      :code_safety_low in classes and matches_low_severity_unwrap?(text) ->
        {:close, :code_safety_low,
         "PR #314: low-severity unwrap/expect findings in cli/bin/fixer/tools " <>
           "paths are advisory-only and no longer dispatched to gitbot-fleet."}

      :migration_rules_fixture in classes and matches_fixture_deprecated?(text) ->
        {:close, :migration_rules_fixture,
         "PR #314: soundness fixture deliberately uses deprecated APIs; " <>
           "migration_rules now respects test/soundness/fixtures/ exemption."}

      :honest_completion_namespaced in classes and matches_namespaced_state?(text) ->
        {:close, :honest_completion_namespaced,
         "PR #314: STATE.a2ml in a namespaced subdir (e.g. .machine_readable/6a2/) " <>
           "now satisfies the check."}

      true ->
        :keep
    end
  end

  defp matches_retired_workflow?(text) do
    String.contains?(text, "missing_workflow") or String.contains?(text, "missing workflow") or
      (String.contains?(text, "workflow_audit") and
         Enum.any?(@retired_workflows, &String.contains?(text, &1)))
  end

  defp matches_low_severity_unwrap?(text) do
    has_rule =
      Enum.any?(
        ~w(unwrap_without_check unwrap_dangerous_default expect_in_hot_path),
        &String.contains?(text, &1)
      )

    has_safe_path =
      Enum.any?(
        ~w(/cli/ /bin/ /tools/ /fixer/ main.rs build.rs),
        &String.contains?(text, &1)
      )

    has_low_marker = String.contains?(text, "severity: low") or String.contains?(text, "low|")

    has_rule and (has_safe_path or has_low_marker)
  end

  defp matches_fixture_deprecated?(text) do
    String.contains?(text, "test/soundness/fixtures/") and
      (String.contains?(text, "deprecated") or String.contains?(text, "migration_rules"))
  end

  defp matches_namespaced_state?(text) do
    String.contains?(text, "no_state_file") and
      (String.contains?(text, ".machine_readable") or
         String.contains?(text, "no STATE.a2ml"))
  end

  # ─── Output ────────────────────────────────────────────────────────────

  defp emit_text(candidates, owner, apply?) do
    Mix.shell().info(
      "Hypatia issue triage — org=#{owner}, #{length(candidates)} closeable candidate(s)" <>
        if(apply?, do: " (--apply mode)", else: " (dry-run)")
    )

    Mix.shell().info(String.duplicate("─", 78))

    candidates
    |> Enum.group_by(& &1.class)
    |> Enum.sort_by(fn {_class, items} -> -length(items) end)
    |> Enum.each(fn {class, items} ->
      Mix.shell().info("\n#{class} (#{length(items)}):")

      items
      |> Enum.take(20)
      |> Enum.each(fn %{issue: i} ->
        repo = i["repository_url"] |> to_string() |> String.split("/repos/") |> List.last()
        Mix.shell().info("  ##{i["number"]} #{repo} — #{i["title"]}")
      end)

      if length(items) > 20 do
        Mix.shell().info("  ... and #{length(items) - 20} more")
      end
    end)

    if not apply? do
      Mix.shell().info(
        "\nDry-run only. Re-run with --apply to close these (and add a comment " <>
          "linking to PR #314). Add --confirm to skip the y/n prompt."
      )
    end
  end

  defp emit_json(candidates, owner, apply?) do
    payload = %{
      "owner" => owner,
      "apply" => apply?,
      "generated_at" => DateTime.utc_now() |> DateTime.to_iso8601(),
      "count" => length(candidates),
      "rows" =>
        Enum.map(candidates, fn %{issue: i, class: c, reason: r} ->
          %{
            "number" => i["number"],
            "repo" =>
              i["repository_url"] |> to_string() |> String.split("/repos/") |> List.last(),
            "title" => i["title"],
            "url" => i["html_url"],
            "class" => Atom.to_string(c),
            "reason" => r
          }
        end)
    }

    IO.puts(Jason.encode!(payload, pretty: true))
  end

  # ─── Apply (close + comment) ───────────────────────────────────────────

  defp apply_closes([], _token, _confirm?) do
    Mix.shell().info("No candidates to close.")
  end

  defp apply_closes(candidates, token, confirm?) do
    if token in [nil, ""] do
      Mix.shell().error("HYPATIA_DISPATCH_PAT is required for --apply.")
      exit({:shutdown, 1})
    end

    proceed? =
      confirm? or
        Mix.shell().yes?(
          "About to close #{length(candidates)} issue(s) across the estate and " <>
            "post an explanatory comment on each. Proceed?"
        )

    unless proceed? do
      Mix.shell().info("Aborted by user.")
      exit({:shutdown, 0})
    end

    {closed, failed} =
      Enum.reduce(candidates, {0, 0}, fn %{issue: i, reason: r}, {ok, bad} ->
        repo = i["repository_url"] |> to_string() |> String.split("/repos/") |> List.last()

        case close_issue(repo, i["number"], r, token) do
          :ok -> {ok + 1, bad}
          :error -> {ok, bad + 1}
        end
      end)

    Mix.shell().info("Closed #{closed} issue(s); #{failed} failed.")

    if failed > 0, do: exit({:shutdown, 1})
  end

  defp close_issue(repo, number, reason, token) do
    comment_body = """
    Auto-closed by `mix hypatia.triage_issues` after PR \
    [hyperpolymath/hypatia#314](https://github.com/hyperpolymath/hypatia/pull/314) \
    landed.

    **Reason**: #{reason}

    This issue was generated by an older Hypatia scan rule that has been \
    either retired, suppressed, or downgraded below the cross-repo dispatch \
    threshold. The same finding will not be regenerated on the next sweep. \
    If you believe this issue represents a real defect that this triage \
    classified wrongly, reopen and tag with `hypatia-triage-disputed`.
    """

    base = "https://api.github.com/repos/#{repo}/issues/#{number}"

    # 1) Post the comment
    comment_url = "#{base}/comments"
    comment_payload = Jason.encode!(%{body: comment_body})

    case curl_post(comment_url, comment_payload, token) do
      :ok ->
        # 2) Close the issue
        close_payload = Jason.encode!(%{state: "closed", state_reason: "not_planned"})

        case curl_patch(base, close_payload, token) do
          :ok ->
            Mix.shell().info("  closed #{repo}##{number}")
            :ok

          :error ->
            Mix.shell().error("  FAILED to close #{repo}##{number}")
            :error
        end

      :error ->
        Mix.shell().error("  FAILED to comment on #{repo}##{number}")
        :error
    end
  end

  defp curl_post(url, body, token) do
    curl_request("POST", url, body, token)
  end

  defp curl_patch(url, body, token) do
    curl_request("PATCH", url, body, token)
  end

  defp curl_request(method, url, body, token) do
    args = [
      "-sS",
      "--max-time",
      "10",
      "-X",
      method,
      "-H",
      "Accept: application/vnd.github+json",
      "-H",
      "X-GitHub-Api-Version: 2022-11-28",
      "-H",
      "Authorization: Bearer #{token}",
      "-d",
      body,
      url
    ]

    case System.cmd("curl", args, stderr_to_stdout: true) do
      {_resp, 0} -> :ok
      _ -> :error
    end
  end
end
