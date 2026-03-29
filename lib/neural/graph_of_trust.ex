# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

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

  @verisimdb_data_path Application.compile_env(:hypatia, :verisimdb_data_path, "data/verisimdb")
  @damping_factor 0.85
  @convergence_threshold 0.001
  @max_iterations 50
  # Future: trust decays 5% per cycle without new data — @decay_rate 0.95

  defstruct nodes: %{}, edges: [], trust_scores: %{}, last_computed: nil,
            cross_repo_edges: [], language_clusters: %{}

  # --- Public API ---

  @doc "Build the trust graph from outcome data, including cross-repo edges."
  def build do
    outcomes = load_all_outcomes()
    recipes = load_all_recipes()

    nodes = build_nodes(outcomes, recipes)
    edges = build_edges(outcomes)

    # Build cross-repo learning edges: repos that share successful
    # recipes are connected, weighted by shared success count
    cross_repo_edges = build_cross_repo_edges(outcomes)

    # Build language clusters: group repos by primary language family
    language_clusters = build_language_clusters(nodes)

    graph = %__MODULE__{
      nodes: nodes,
      edges: edges ++ cross_repo_edges,
      cross_repo_edges: cross_repo_edges,
      language_clusters: language_clusters
    }

    graph = compute_trust(graph)

    Logger.info(
      "Graph of Trust built: #{map_size(graph.nodes)} nodes, " <>
      "#{length(graph.edges)} edges (#{length(cross_repo_edges)} cross-repo)"
    )
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

  @doc """
  Find repos similar to the given repo based on shared successful recipes.

  Returns [{repo, similarity_score}] sorted by similarity descending.
  Uses the cross-repo edge weights: repos that share many successful
  recipes are considered more similar.
  """
  def similar_repos(%__MODULE__{cross_repo_edges: edges, trust_scores: scores}, repo) do
    edges
    |> Enum.filter(fn {src, _tgt, _w} -> src == repo end)
    |> Enum.map(fn {_src, tgt, weight} ->
      trust = Map.get(scores, tgt, 0.5)
      {tgt, weight * trust}
    end)
    |> Enum.sort_by(fn {_repo, score} -> -score end)
  end

  @doc """
  Get repos in the same language cluster as the given repo.

  Returns the list of repo names that share the same language family,
  useful for language-aware drift policies.
  """
  def repos_in_language_cluster(%__MODULE__{language_clusters: clusters}, repo) do
    Enum.find_value(clusters, [], fn {_family, repos} ->
      if repo in repos, do: repos -- [repo], else: nil
    end)
  end

  @doc """
  Get the cross-repo learning edges for a specific repo.

  Returns [{source_repo, target_repo, weight}] where weight represents
  the strength of the learning transfer relationship.
  """
  def cross_repo_edges_for(%__MODULE__{cross_repo_edges: edges}, repo) do
    Enum.filter(edges, fn {src, tgt, _w} -> src == repo or tgt == repo end)
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

  # Build edges between repos that share successful recipe applications.
  # Weight = number of shared successful recipes between two repos,
  # normalized by total unique recipes each repo has used.
  defp build_cross_repo_edges(outcomes) do
    # Group successful outcomes by repo, collecting recipe sets
    repo_recipes =
      outcomes
      |> Enum.filter(&(Map.get(&1, "outcome") == "success"))
      |> Enum.group_by(&Map.get(&1, "repo", "unknown"))
      |> Enum.map(fn {repo, repo_outcomes} ->
        recipes =
          repo_outcomes
          |> Enum.map(&(Map.get(&1, "recipe_id", Map.get(&1, "pattern", "unknown"))))
          |> MapSet.new()
        {repo, recipes}
      end)
      |> Enum.reject(fn {repo, _} -> repo == "unknown" end)

    # Build pairwise edges based on recipe overlap (Jaccard similarity)
    for {repo_a, recipes_a} <- repo_recipes,
        {repo_b, recipes_b} <- repo_recipes,
        repo_a < repo_b do
      intersection = MapSet.intersection(recipes_a, recipes_b) |> MapSet.size()

      if intersection > 0 do
        union = MapSet.union(recipes_a, recipes_b) |> MapSet.size()
        jaccard = intersection / max(union, 1)

        # Bidirectional edges with Jaccard similarity as weight
        [{repo_a, repo_b, jaccard}, {repo_b, repo_a, jaccard}]
      else
        []
      end
    end
    |> List.flatten()
  end

  # Build language clusters from repo scan data.
  # Groups repos by their detected primary language family.
  defp build_language_clusters(nodes) do
    language_families = Hypatia.CrossRepoLearning.language_families()

    repos =
      nodes
      |> Enum.filter(fn {_id, type} -> type == :repo end)
      |> Enum.map(fn {id, _} -> id end)

    repos
    |> Enum.reduce(%{}, fn repo, clusters ->
      lang = detect_repo_language(repo)
      family = Map.get(language_families, lang, :unknown)

      if family != :unknown do
        Map.update(clusters, family, [repo], fn existing -> [repo | existing] end)
      else
        clusters
      end
    end)
  end

  # Detect primary language for a repo from scan data
  defp detect_repo_language(repo) do
    scan_path = Path.join([Path.expand(@verisimdb_data_path), "scans", "#{repo}.json"])

    case File.read(scan_path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} ->
            cond do
              Map.has_key?(data, "primary_language") ->
                String.downcase(Map.get(data, "primary_language", "unknown"))

              Map.has_key?(data, "languages") ->
                data
                |> Map.get("languages", %{})
                |> Enum.max_by(fn {_lang, count} -> count end, fn -> {"unknown", 0} end)
                |> elem(0)
                |> String.downcase()

              true -> "unknown"
            end

          {:error, _} -> "unknown"
        end

      {:error, _} -> "unknown"
    end
  end

  defp load_all_outcomes do
    outcomes_dir = Path.join(Path.expand(@verisimdb_data_path), "outcomes")

    case File.ls(outcomes_dir) do
      {:ok, files} ->
        # Stream JSONL files line-by-line to avoid loading entire files
        # into memory. Prevents timeout cascades on large datasets.
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.flat_map(fn f ->
          path = Path.join(outcomes_dir, f)

          path
          |> File.stream!()
          |> Stream.map(fn line ->
            case Jason.decode(String.trim(line)) do
              {:ok, record} -> record
              _ -> nil
            end
          end)
          |> Stream.reject(&is_nil/1)
          |> Enum.to_list()
        end)

      _ ->
        []
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
