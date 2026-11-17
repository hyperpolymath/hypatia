# SPDX-License-Identifier: MPL-2.0
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.FleetEnrolmentIngestor do
  @moduledoc """
  Repo-level check that emits the `MissingFleetEnrolment` category.

  A repository whose pull requests carry the Hypatia verdict grid (it has the
  fork governance workflow, or a workflow that calls standards'
  `hypatia-scan-reusable.yml`) but has no `.gitbot-fleet/enrolment.json` shows
  no gitbot-fleet rows at all: the grid cannot say which members apply. The
  fix is gitbot-fleet's `scripts/fix-fleet-enrolment.sh`, which writes the
  manifest and wires the producing job (gitbot-fleet `docs/REPORT-BACK.adoc`
  section 6). `scripts/fix-script-registry.json` in gitbot-fleet maps this
  category to that script.

  The finding is routed at review confidence (0.88, the 0.85-0.94 tier),
  because the fixer edits workflows and must never be applied unreviewed. In
  gitbot-fleet's `dispatch-runner.sh` that tier does not run the fixer: it
  records the finding under `shared-context/findings/pending/` with
  `needs_pr: true`, and `process-review-findings.sh` turns pending findings
  into one GitHub issue per repository. As of 2026-11-17 nothing in the fleet
  turns a pending finding into a pull request, so a person (or a future
  pull-request step) runs the fixer. The fixer itself refuses to rewrite an
  existing manifest and never edits a lockfile.

  Also as of 2026-11-17, that whole path is quarantined: `dispatch-runner.sh`
  and live issue publication exit 78 until directive enforcement is qualified
  (gitbot-fleet `docs/AUTOMATION-QUARANTINE.adoc`). This check only emits the
  finding; it authorises no write.

  The fixer puts the producing job only in `fork-governance.yml`. A repository
  that calls the grid reusable directly is still flagged, but as not
  auto-fixable, with a remediation that names fork governance adoption first.

  Not flagged:

    * a repository with no grid lane (nothing would read the manifest);
    * a repository that already has the manifest, whatever it enrols (the
      manifest is the owner's; an empty or partial enrolment is a choice);
    * the `007` repository, which estate policy puts out of scope for every
      scan (standards CLAUDE.md, licence category 2).
  """

  @category "MissingFleetEnrolment"
  @confidence 0.88
  @manifest ".gitbot-fleet/enrolment.json"
  @governance_workflow ".github/workflows/fork-governance.yml"
  @grid_reusable "hypatia-scan-reusable.yml"
  @out_of_scope ["007"]

  @doc "The category this check emits."
  def category, do: @category

  @doc """
  Check one repository. Returns `{:ok, patterns}`; the list is empty when the
  repository is enrolled, has no grid lane, or is out of scope.
  """
  def local_scan(repo_path, repo_name) when is_binary(repo_path) and is_binary(repo_name) do
    cond do
      repo_name in @out_of_scope -> {:ok, []}
      not File.dir?(repo_path) -> {:ok, []}
      enrolled?(repo_path) -> {:ok, []}
      not grid_lane?(repo_path) -> {:ok, []}
      true -> {:ok, [pattern(repo_name, governed?(repo_path))]}
    end
  end

  @doc """
  Check many repositories, given as `{repo_name, repo_path}` pairs. Returns
  `{patterns, stats}` in the shape `ScorecardIngestor.batch_local_scan/1` uses.
  """
  def batch_local_scan(repo_paths) when is_list(repo_paths) do
    patterns =
      Enum.flat_map(repo_paths, fn {repo_name, repo_path} ->
        {:ok, found} = local_scan(repo_path, repo_name)
        found
      end)

    {patterns, %{total_repos: length(repo_paths), total_findings: length(patterns)}}
  end

  @doc """
  True when the manifest is a regular file. A directory, a dangling path or a
  symlink to elsewhere is not a manifest the grid can read.
  """
  def enrolled?(repo_path) do
    path = Path.join(repo_path, @manifest)

    case File.lstat(path) do
      {:ok, %File.Stat{type: :regular}} -> true
      _ -> false
    end
  end

  @doc """
  True when the repository's pull requests render the verdict grid: it has
  the fork governance workflow, or a workflow that calls the Hypatia grid
  reusable.
  """
  def grid_lane?(repo_path) do
    File.regular?(Path.join(repo_path, @governance_workflow)) or
      repo_path
      |> Path.join(".github/workflows/*.{yml,yaml}")
      |> Path.wildcard()
      |> Enum.any?(&calls_grid_reusable?/1)
  end

  defp calls_grid_reusable?(workflow) do
    case File.read(workflow) do
      {:ok, text} ->
        text
        |> String.split("\n")
        |> Enum.any?(fn line ->
          trimmed = String.trim_leading(line)

          not String.starts_with?(trimmed, "#") and String.contains?(trimmed, "uses:") and
            String.contains?(trimmed, @grid_reusable)
        end)

      _ ->
        false
    end
  end

  # The fixer inserts the producing job into fork-governance.yml only; a repo
  # that calls the grid reusable directly must adopt fork governance first,
  # and an enrolment without a producer would turn every row orange.
  defp governed?(repo_path), do: File.regular?(Path.join(repo_path, @governance_workflow))

  defp pattern(repo_name, governed?) do
    %{
      "id" => "FE-001-#{fingerprint(repo_name)}",
      "category" => @category,
      "severity" => "Low",
      "description" =>
        "#{repo_name} renders the Hypatia verdict grid but has no #{@manifest}: " <>
          "no gitbot-fleet member can report back on its pull requests",
      "pa_rule" => "FE001",
      "remediation" => remediation(governed?),
      "auto_fixable" => governed?,
      "confidence" => @confidence,
      "repos_affected_list" => [repo_name],
      "repos_affected" => 1,
      "occurrences" => 1,
      "source" => "fleet-enrolment",
      "triangle_tier" => "eliminate"
    }
  end

  defp remediation(true) do
    "Run gitbot-fleet scripts/fix-fleet-enrolment.sh on the branch that bumps the " <>
      "standards pins; review the manifest it writes (docs/REPORT-BACK.adoc section 6)."
  end

  defp remediation(false) do
    "Adopt fork governance first (standards 3-practice/fork-governance/ADOPTION.adoc): " <>
      "the fleet job that produces report-backs lives in fork-governance.yml, and a manifest " <>
      "without that job would show every enrolled row as missing. Then run " <>
      "gitbot-fleet scripts/fix-fleet-enrolment.sh."
  end

  defp fingerprint(text) do
    text
    |> String.downcase()
    |> String.replace(~r/[^a-z0-9]+/, "-")
    |> String.trim("-")
    |> String.slice(0, 20)
  end
end
