# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.Neural.GraphOfTrust do
  @moduledoc """
  Graph of Trust for Hypatia.

  Builds and maintains a weighted directed graph of trust relationships:
  - Repos: trust score based on scan results and fix success rate
  - Bots: trust score based on dispatch success/failure history
  - Recipes: trust score based on outcome history (success/failure/FP)
  - Contributors: trust score based on fix quality

  Trust propagation uses a PageRank-style iterative algorithm:
  - Successful fixes propagate trust forward
  - Failures reduce trust
  - False positives reduce trust more heavily
  - Trust decays over time (recency weighting)

  The graph enables:
  - Routing dispatches to trusted bots
  - Prioritizing recipes with high trust
  - Flagging repos with declining trust for attention
  - Cross-repo learning through trust-weighted patterns
  """

  require Logger

  @verisimdb_data_path "~/Documents/hyperpolymath-repos/verisimdb-data"
  @damping_factor 0.85
  @convergence_threshold 0.001
  @max_iterations 50
  # Future: trust decays 5% per cycle without new data — @decay_rate 0.95

  defstruct nodes: %{}, edges: [], trust_scores: %{}, last_computed: nil

  # --- Public API ---

  @doc "Build the trust graph from outcome data"
  def build do
    outcomes = load_all_outcomes()
    recipes = load_all_recipes()

    nodes = build_nodes(outcomes, recipes)
    edges = build_edges(outcomes)

    graph = %__MODULE__{nodes: nodes, edges: edges}
    graph = compute_trust(graph)

    Logger.info("Graph of Trust built: #{map_size(graph.nodes)} nodes, #{length(graph.edges)} edges")
    graph
  end

  @doc "Get trust score for a specific entity"
  def trust_score(%__MODULE__{trust_scores: scores}, entity_id) do
    Map.get(scores, entity_id, 0.5)  # Default trust = 0.5 (neutral)
  end

  @doc "Get the most trusted recipes (for dispatch prioritization)"
  def trusted_recipes(%__MODULE__{trust_scores: scores, nodes: nodes}) do
    nodes
    |> Enum.filter(fn {_id, type} -> type == :recipe end)
    |> Enum.map(fn {id, _} -> {id, Map.get(scores, id, 0.5)} end)
    |> Enum.sort_by(fn {_id, score} -> -score end)
  end

  @doc "Get the most trusted bots (for dispatch routing)"
  def trusted_bots(%__MODULE__{trust_scores: scores, nodes: nodes}) do
    nodes
    |> Enum.filter(fn {_id, type} -> type == :bot end)
    |> Enum.map(fn {id, _} -> {id, Map.get(scores, id, 0.5)} end)
    |> Enum.sort_by(fn {_id, score} -> -score end)
  end

  @doc "Get repos with declining trust (need attention)"
  def repos_needing_attention(%__MODULE__{trust_scores: scores, nodes: nodes}) do
    nodes
    |> Enum.filter(fn {_id, type} -> type == :repo end)
    |> Enum.map(fn {id, _} -> {id, Map.get(scores, id, 0.5)} end)
    |> Enum.filter(fn {_id, score} -> score < 0.4 end)
    |> Enum.sort_by(fn {_id, score} -> score end)
  end

  # --- Trust Computation (PageRank-style) ---

  defp compute_trust(%__MODULE__{nodes: nodes} = graph) when map_size(nodes) == 0, do: graph

  defp compute_trust(%__MODULE__{nodes: nodes, edges: edges} = graph) do
    n = map_size(nodes)

    # Initialize all nodes to 1/n
    initial_score = 1.0 / max(n, 1)
    scores = Map.new(Map.keys(nodes), fn id -> {id, initial_score} end)

    # Build adjacency: source -> [{target, weight}]
    adjacency = Enum.reduce(edges, %{}, fn {src, tgt, weight}, acc ->
      Map.update(acc, src, [{tgt, weight}], fn existing -> [{tgt, weight} | existing] end)
    end)

    # Iterative convergence
    final_scores = iterate_trust(scores, adjacency, n, 0)

    %{graph | trust_scores: final_scores, last_computed: DateTime.utc_now()}
  end

  defp iterate_trust(scores, _adjacency, _n, iteration) when iteration >= @max_iterations do
    scores
  end

  defp iterate_trust(scores, adjacency, n, iteration) do
    new_scores = Map.new(scores, fn {node_id, _old_score} ->
      # Sum of weighted incoming trust
      incoming = Enum.reduce(adjacency, 0.0, fn {src, targets}, acc ->
        case Enum.find(targets, fn {tgt, _w} -> tgt == node_id end) do
          {_tgt, weight} ->
            src_score = Map.get(scores, src, 0.0)
            out_degree = length(Map.get(adjacency, src, []))
            acc + (src_score * weight / max(out_degree, 1))
          nil ->
            acc
        end
      end)

      new_score = (1 - @damping_factor) / n + @damping_factor * incoming
      {node_id, new_score}
    end)

    # Check convergence
    max_delta = Enum.reduce(new_scores, 0.0, fn {id, new}, acc ->
      old = Map.get(scores, id, 0.0)
      max(acc, abs(new - old))
    end)

    if max_delta < @convergence_threshold do
      normalize_scores(new_scores)
    else
      iterate_trust(new_scores, adjacency, n, iteration + 1)
    end
  end

  defp normalize_scores(scores) when map_size(scores) == 0, do: scores

  defp normalize_scores(scores) do
    max_score = scores |> Map.values() |> Enum.max() |> max(0.001)
    Map.new(scores, fn {k, v} -> {k, min(v / max_score, 1.0)} end)
  end

  # --- Graph Construction ---

  defp build_nodes(outcomes, recipes) do
    repo_nodes = outcomes |> Enum.map(fn o -> {Map.get(o, "repo"), :repo} end)
    bot_nodes = outcomes |> Enum.map(fn o -> {Map.get(o, "bot", "unknown"), :bot} end)
    recipe_nodes = recipes |> Enum.map(fn r -> {Map.get(r, "id"), :recipe} end)

    (repo_nodes ++ bot_nodes ++ recipe_nodes)
    |> Enum.reject(fn {id, _} -> is_nil(id) end)
    |> Map.new()
  end

  defp build_edges(outcomes) do
    Enum.flat_map(outcomes, fn outcome ->
      repo = Map.get(outcome, "repo", "unknown")
      bot = Map.get(outcome, "bot", "unknown")
      recipe = Map.get(outcome, "recipe_id", Map.get(outcome, "pattern", "unknown"))
      success = Map.get(outcome, "outcome") == "success"

      weight = if success, do: 1.0, else: -0.5
      fp_weight = if Map.get(outcome, "outcome") == "false_positive", do: -1.0, else: weight

      [
        {recipe, repo, fp_weight},    # recipe fixes repo
        {bot, recipe, fp_weight},     # bot executes recipe
        {repo, bot, weight * 0.5}     # repo provides context to bot
      ]
    end)
  end

  # --- Data Loading ---

  defp load_all_outcomes do
    outcomes_dir = Path.join(Path.expand(@verisimdb_data_path), "outcomes")

    case File.ls(outcomes_dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.flat_map(fn f ->
          path = Path.join(outcomes_dir, f)
          case File.read(path) do
            {:ok, content} ->
              content
              |> String.split("\n", trim: true)
              |> Enum.map(fn line ->
                case Jason.decode(line) do
                  {:ok, record} -> record
                  _ -> nil
                end
              end)
              |> Enum.reject(&is_nil/1)
            _ -> []
          end
        end)
      _ -> []
    end
  end

  defp load_all_recipes do
    recipes_dir = Path.join(Path.expand(@verisimdb_data_path), "recipes")

    case File.ls(recipes_dir) do
      {:ok, files} ->
        files
        |> Enum.filter(&String.ends_with?(&1, ".json"))
        |> Enum.map(fn f ->
          path = Path.join(recipes_dir, f)
          case File.read(path) do
            {:ok, content} ->
              case Jason.decode(content) do
                {:ok, recipe} -> recipe
                _ -> nil
              end
            _ -> nil
          end
        end)
        |> Enum.reject(&is_nil/1)
      _ -> []
    end
  end
end
