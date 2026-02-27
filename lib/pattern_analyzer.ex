# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.PatternAnalyzer do
  @moduledoc """
  Analyzes scan results from verisimdb, detects patterns, routes through
  the safety triangle, and dispatches to the gitbot-fleet.

  Pipeline:
  1. Fetch scans from verisimdb-data
  2. Sync pattern registry (deduplicate findings into canonical patterns)
  3. For each pattern+repo, route through safety triangle
  4. Dispatch routed actions to fleet bots
  5. Write dispatch manifest (JSONL for execution layer)
  6. Return summary
  """

  alias Hypatia.VerisimdbConnector
  alias Hypatia.PatternRegistry
  alias Hypatia.TriangleRouter
  alias Hypatia.FleetDispatcher
  alias Hypatia.DispatchManifest
  alias Hypatia.Neural.Coordinator, as: NeuralCoordinator

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

    # Step 3: Route each pattern+repo through the safety triangle.
    # Tag each pattern with its routed_repo so manifest entries are 1:1.
    routed =
      Enum.flat_map(patterns, fn pattern ->
        repos = Map.get(pattern, "repos_affected_list", [])

        Enum.map(repos, fn repo ->
          language = Map.get(repo_languages, repo, "unknown")
          tagged_pattern = Map.put(pattern, "routed_repo", repo)
          TriangleRouter.route(tagged_pattern, repo, language)
        end)
      end)

    # Step 4: Enhance dispatch decisions with neural intelligence.
    # The coordinator provides novelty detection, trust-weighted routing,
    # and aggregated confidence from all 5 neural networks.
    routed = enhance_with_neural(routed)

    # Step 5: Dispatch routed actions to fleet
    Enum.each(routed, &FleetDispatcher.dispatch_routed_action/1)

    # Step 6: Write dispatch manifest (JSONL — executable by dispatch-runner.sh)
    {:ok, manifest_path, manifest_stats} = DispatchManifest.write(routed)

    Logger.info(
      "Manifest: #{manifest_stats.auto_execute} auto, #{manifest_stats.review} review, " <>
        "#{manifest_stats.report_only} report → #{manifest_path}"
    )

    # Step 7: Generate and return summary
    summary = generate_summary(routed, scans) |> Map.put(:manifest_path, manifest_path)
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

  # Enhance routed actions with neural intelligence. Consults the Neural.Coordinator
  # for each finding to get novelty detection, trust-weighted bot preferences, and
  # aggregated confidence from all 5 networks (MoE, RBF, LSM, ESN, Graph of Trust).
  #
  # Novel findings are demoted to :control (report_only) regardless of recipe confidence.
  # For non-novel findings, the neural aggregated confidence can override the recipe's
  # static confidence when the neural prediction is lower (conservative adjustment).
  defp enhance_with_neural(routed_actions) do
    Enum.map(routed_actions, fn action ->
      pattern = extract_pattern(action)

      case neural_recommendation(pattern) do
        {:ok, rec} ->
          apply_neural_recommendation(action, rec)

        :unavailable ->
          # Neural coordinator not running — pass through unchanged
          action
      end
    end)
  end

  # Get a dispatch recommendation from the neural coordinator.
  # Returns {:ok, recommendation} or :unavailable if the coordinator is down.
  defp neural_recommendation(pattern) do
    try do
      case NeuralCoordinator.dispatch_recommendation(pattern) do
        %{} = rec -> {:ok, rec}
        _ -> :unavailable
      end
    catch
      :exit, _ -> :unavailable
    end
  end

  # Apply neural recommendation to a routed action.
  # - Novel findings: demote to :control (human review required)
  # - Non-novel: adjust confidence downward if neural prediction is lower (conservative)
  defp apply_neural_recommendation(action, %{is_novel: true} = _rec) do
    # Novel finding — force to control tier for human review
    pattern = extract_pattern(action)
    Logger.info("Neural: novel finding detected for #{Map.get(pattern, "id", "?")}, demoting to control")
    {:control, Map.put(pattern, "neural_novel", true)}
  end

  defp apply_neural_recommendation({:eliminate, recipe, pattern}, %{confidence: neural_conf} = rec) do
    recipe_conf = Map.get(recipe, "confidence", 0.0)

    # Use the lower of recipe confidence and neural confidence (conservative)
    effective_conf = min(recipe_conf, neural_conf)
    updated_recipe = recipe
      |> Map.put("confidence", effective_conf)
      |> Map.put("neural_confidence", neural_conf)
      |> Map.put("preferred_bots", Map.get(rec, :preferred_bots, []))

    {:eliminate, updated_recipe, pattern}
  end

  defp apply_neural_recommendation({:substitute, recipe, pattern}, %{confidence: neural_conf} = rec) do
    updated_recipe = recipe
      |> Map.put("neural_confidence", neural_conf)
      |> Map.put("preferred_bots", Map.get(rec, :preferred_bots, []))

    {:substitute, updated_recipe, pattern}
  end

  defp apply_neural_recommendation(action, _rec), do: action

  defp extract_pattern({:eliminate, _recipe, pattern}), do: pattern
  defp extract_pattern({:substitute, _recipe, pattern}), do: pattern
  defp extract_pattern({:control, pattern}), do: pattern

  defp build_language_map(scans) do
    Enum.reduce(scans, %{}, fn scan, acc ->
      language = Map.get(scan.scan, "language", "unknown")
      Map.put(acc, scan.repo, language)
    end)
  end
end
