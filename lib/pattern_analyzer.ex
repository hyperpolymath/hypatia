# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.PatternAnalyzer do
  @moduledoc """
  Analyzes scan results from verisimdb, detects patterns, routes through
  the safety triangle, and dispatches to the gitbot-fleet.

  Pipeline:
  1. Fetch scans from verisimdb-data
  2. Sync pattern registry (deduplicate findings into canonical patterns)
  3. For each pattern, find best recipe via triangle router
  4. Dispatch routed actions to fleet bots
  5. Return summary
  """

  alias Hypatia.VerisimdbConnector
  alias Hypatia.PatternRegistry
  alias Hypatia.TriangleRouter
  alias Hypatia.FleetDispatcher

  require Logger

  @doc """
  Run the full analysis pipeline: scan → patterns → triangle → dispatch.
  """
  def analyze_all_scans do
    scans = VerisimdbConnector.fetch_all_scans()
    Logger.info("Loaded #{length(scans)} scan results")

    # Step 1: Sync pattern registry from scan data
    {:ok, registry} = PatternRegistry.sync_from_scans(scans)
    patterns = Map.get(registry, "patterns", %{}) |> Map.values()
    Logger.info("Pattern registry: #{length(patterns)} canonical patterns")

    # Step 2: Build repo→language map from scans
    repo_languages = build_language_map(scans)

    # Step 3: Route each pattern through the safety triangle
    routed =
      Enum.flat_map(patterns, fn pattern ->
        repos = Map.get(pattern, "repos_affected_list", [])

        Enum.map(repos, fn repo ->
          language = Map.get(repo_languages, repo, "unknown")
          TriangleRouter.route(pattern, repo, language)
        end)
      end)

    # Step 4: Dispatch routed actions to fleet
    Enum.each(routed, &FleetDispatcher.dispatch_routed_action/1)

    # Step 5: Generate and return summary
    summary = generate_summary(routed, scans)
    Logger.info("Pipeline complete: #{summary.total_actions} actions dispatched")

    {:ok, summary}
  end

  @doc """
  Generate a summary of the analysis pipeline results.
  """
  def generate_summary(routed_actions, scans) do
    eliminate_count = Enum.count(routed_actions, fn
      {:eliminate, _, _} -> true
      _ -> false
    end)

    substitute_count = Enum.count(routed_actions, fn
      {:substitute, _, _} -> true
      _ -> false
    end)

    control_count = Enum.count(routed_actions, fn
      {:control, _} -> true
      _ -> false
    end)

    total_weak_points =
      scans
      |> Enum.map(fn scan ->
        Map.get(scan.scan, "weak_points", []) |> length()
      end)
      |> Enum.sum()

    auto_execute =
      Enum.count(routed_actions, fn
        {:eliminate, recipe, _} ->
          Map.get(recipe, "confidence", 0) >= 0.95

        _ ->
          false
      end)

    %{
      total_repos: length(scans),
      total_weak_points: total_weak_points,
      total_actions: length(routed_actions),
      triangle_breakdown: %{
        eliminate: eliminate_count,
        substitute: substitute_count,
        control: control_count
      },
      auto_executable: auto_execute,
      review_required: eliminate_count + substitute_count - auto_execute
    }
  end

  @doc """
  Process findings through the fleet dispatcher (legacy compatibility).
  """
  def process_findings(findings) do
    Enum.each(findings, fn finding ->
      case FleetDispatcher.dispatch_finding(finding) do
        {:ok, _} -> :ok
        {:error, reason} -> Logger.error("Dispatch failed: #{inspect(reason)}")
      end
    end)
  end

  # --- Private ---

  defp build_language_map(scans) do
    Enum.reduce(scans, %{}, fn scan, acc ->
      language = Map.get(scan.scan, "language", "unknown")
      Map.put(acc, scan.repo, language)
    end)
  end
end
