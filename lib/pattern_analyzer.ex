# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.PatternAnalyzer do
  @moduledoc """
  Analyzes scan results from verisim, detects patterns, routes through
  the safety triangle, and dispatches to the gitbot-fleet.

  Pipeline:
  1. Fetch scans from verisim-data
  2. Sync pattern registry (deduplicate findings into canonical patterns)
  3. For each pattern+repo, route through safety triangle
  4. Dispatch routed actions to fleet bots
  5. Write dispatch manifest (JSONL for execution layer)
  6. Return summary
  """

  alias Hypatia.VerisimConnector
  alias Hypatia.PatternRegistry
  alias Hypatia.TriangleRouter
  alias Hypatia.FleetDispatcher
  alias Hypatia.DispatchManifest
  alias Hypatia.HistoricalTrends
  alias Hypatia.Neural.Coordinator, as: NeuralCoordinator
  alias Hypatia.ScorecardIngestor

  require Logger

  @doc """
  Run the full analysis pipeline: scan → patterns → triangle → dispatch.
  """
  def analyze_all_scans do
    scans = VerisimConnector.fetch_all_scans()
    Logger.info("Loaded #{length(scans)} scan results")

    # Step 0: Record scan snapshot to scan-history. Append-only; no failure
    # here should block the pipeline — the rest of the run is still useful
    # even if history write is skipped.
    case HistoricalTrends.record_scan_snapshot(scans) do
      {:ok, scan_id, _} -> Logger.info("Recorded scan snapshot #{scan_id}")
      other -> Logger.warning("Scan snapshot not recorded: #{inspect(other)}")
    end

    # Step 1: Sync pattern registry from scan data
    {:ok, registry} = PatternRegistry.sync_from_scans(scans)
    patterns = Map.get(registry, "patterns", %{}) |> Map.values()
    Logger.info("Pattern registry: #{length(patterns)} canonical patterns")

    # Step 2: Build repo→language map from scans
    repo_languages = build_language_map(scans)

    # Step 2b: Run Scorecard local checks against scanned repos.
    # Ingests OpenSSF's 20 well-documented checks directly rather than
    # rediscovering them from first principles.
    scorecard_patterns = run_scorecard_checks(scans)
    patterns = patterns ++ scorecard_patterns
    Logger.info("Scorecard: #{length(scorecard_patterns)} repo-level findings added (#{length(patterns)} total)")

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

    # Step 6: Write dispatch manifest (JSONL -- executable by dispatch-runner.sh)
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
  Run the analysis pipeline in configurable batches.

  Options:
    - `:batch_size` -- number of repos to process per batch (default: 50)
    - `:max_concurrency` -- parallel workers per batch (default: 4)
    - `:on_batch_complete` -- callback `fn batch_num, batch_result -> :ok` (optional)
    - `:repos` -- specific repo list to process (default: all from scans)

  Returns `{:ok, %{batches: N, total_actions: N, summary: ...}}`.
  """
  def analyze_batch(opts \\ []) do
    batch_size = Keyword.get(opts, :batch_size, 50)
    max_concurrency = Keyword.get(opts, :max_concurrency, 4)
    on_complete = Keyword.get(opts, :on_batch_complete, fn _n, _r -> :ok end)
    repo_filter = Keyword.get(opts, :repos, nil)

    scans = VerisimConnector.fetch_all_scans()
    Logger.info("[Batch] Loaded #{length(scans)} scan results")

    # Filter repos if specified
    scans =
      if repo_filter do
        Enum.filter(scans, fn s -> Map.get(s, :repo, "") in repo_filter end)
      else
        scans
      end

    # Build shared state
    {:ok, registry} = PatternRegistry.sync_from_scans(scans)
    patterns = Map.get(registry, "patterns", %{}) |> Map.values()
    repo_languages = build_language_map(scans)
    scorecard_patterns = run_scorecard_checks(scans)
    all_patterns = patterns ++ scorecard_patterns

    # Chunk repos and process in batches
    all_repos =
      all_patterns
      |> Enum.flat_map(fn p -> Map.get(p, "repos_affected_list", []) end)
      |> Enum.uniq()

    batches = Enum.chunk_every(all_repos, batch_size)
    Logger.info("[Batch] Processing #{length(all_repos)} repos in #{length(batches)} batches (size=#{batch_size}, workers=#{max_concurrency})")

    all_routed =
      batches
      |> Enum.with_index(1)
      |> Enum.flat_map(fn {batch_repos, batch_num} ->
        Logger.info("[Batch #{batch_num}/#{length(batches)}] Processing #{length(batch_repos)} repos")

        # Filter patterns to those affecting this batch's repos
        batch_patterns =
          Enum.filter(all_patterns, fn p ->
            repos = Map.get(p, "repos_affected_list", [])
            Enum.any?(repos, &(&1 in batch_repos))
          end)

        # Process repos in parallel within the batch
        routed =
          batch_patterns
          |> Task.async_stream(
            fn pattern ->
              repos = Map.get(pattern, "repos_affected_list", [])
              |> Enum.filter(&(&1 in batch_repos))

              Enum.map(repos, fn repo ->
                language = Map.get(repo_languages, repo, "unknown")
                tagged = Map.put(pattern, "routed_repo", repo)
                TriangleRouter.route(tagged, repo, language)
              end)
            end,
            max_concurrency: max_concurrency,
            timeout: 30_000,
            on_timeout: :kill_task
          )
          |> Enum.flat_map(fn
            {:ok, results} -> results
            {:exit, _reason} -> []
          end)

        on_complete.(batch_num, %{repo_count: length(batch_repos), actions: length(routed)})
        routed
      end)

    # Neural enhancement + dispatch
    all_routed = enhance_with_neural(all_routed)
    Enum.each(all_routed, &FleetDispatcher.dispatch_routed_action/1)
    {:ok, manifest_path, manifest_stats} = DispatchManifest.write(all_routed)

    summary = generate_summary(all_routed, scans)
    |> Map.merge(%{
      manifest_path: manifest_path,
      batches: length(batches),
      batch_size: batch_size,
      manifest_stats: manifest_stats
    })

    Logger.info("[Batch] Complete: #{length(batches)} batches, #{summary.total_actions} actions")
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
          # Neural coordinator not running -- pass through unchanged
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
    # Novel finding -- force to control tier for human review
    pattern = extract_pattern(action)
    Logger.info("Neural: novel finding detected for #{Map.get(pattern, "id", "?")}, demoting to control")
    {:control, Map.put(pattern, "neural_novel", true)}
  end

  defp apply_neural_recommendation({:eliminate, recipe, pattern}, %{confidence: neural_conf} = rec) do
    recipe_conf = Map.get(recipe, "confidence", 0.0)

    # Only let neural confidence override if it's meaningfully trained.
    # Untrained MoE returns sigmoid(1.0) ~= 0.73 -- don't let that drag down
    # a proven recipe at 0.99. Neural override kicks in once confidence
    # diverges from the default sigmoid output (i.e., the network has learned).
    neural_is_trained = neural_conf < 0.70 or neural_conf > 0.76
    effective_conf = if neural_is_trained, do: min(recipe_conf, neural_conf), else: recipe_conf

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

  # Run Scorecard local checks against all repos that have scan data.
  # Resolves repo paths from scan program_path or falls back to standard locations.
  defp run_scorecard_checks(scans) do
    repo_paths =
      scans
      |> Enum.map(fn scan ->
        path = Map.get(scan.scan, "program_path", "")
        resolved = if is_binary(path) and path != "" and path != "." and File.dir?(path) do
          path
        else
          Path.join(System.get_env("HYPATIA_REPOS_DIR", File.cwd!()), scan.repo)
        end
        {scan.repo, resolved}
      end)
      |> Enum.filter(fn {_repo, path} -> File.dir?(path) end)

    {patterns, stats} = ScorecardIngestor.batch_local_scan(repo_paths)

    Logger.info(
      "Scorecard local scan: #{stats.total_repos} repos, " <>
      "#{stats.total_findings} findings -- #{inspect(stats.by_check)}"
    )

    patterns
  end
end
