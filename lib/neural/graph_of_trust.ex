# SPDX-License-Identifier: MPL-2.0
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
  - Qualified successful fixes propagate positive support
  - Inventory-only and negative-only entities remain neutral (0.5)
  - Negative outcomes remain visible but do not create positive edges

  Scores are relative graph rankings, not calibrated success probabilities or
  authenticated proof. Negative penalties and time decay are not implemented.

  The graph enables:
  - Routing dispatches to trusted bots
  - Prioritizing recipes with high trust
  - Flagging repos with declining trust for attention
  - Cross-repo learning through trust-weighted patterns
  """

  require Logger

  alias Hypatia.OutcomeTracker

  @damping_factor 0.85
  @convergence_threshold 0.001
  @max_iterations 50
  # Future: trust decays 5% per cycle without new data -- @decay_rate 0.95

  defstruct nodes: %{},
            edges: [],
            trust_scores: %{},
            last_computed: nil,
            cross_repo_edges: [],
            language_clusters: %{}

  # --- Public API ---

  @doc """
  Build the trust graph from qualified outcome data, including cross-repo edges.

  Unqualified observations remain in the outcome log but contribute neither
  outcome-derived nodes nor edges to the graph. Outcome subjects must have
  nonblank string repo, bot and recipe identity (recipe_id or canonical pattern).
  Missing and malformed subjects remain audit rows and create no partial edges.
  Qualified negative evidence
  retains the existing graph policy: it does not create positive edges.
  """
  def build, do: build_from_store(Hypatia.Paths.verisimdb_data())

  @doc "Build trust and language context from one captured source directory."
  def build_from_store(data_root) do
    source = Path.expand(data_root)
    build_qualified(load_all_outcomes(source), load_all_recipes(source), source)
  end

  @doc false
  def build(outcomes, recipes) when is_list(outcomes) and is_list(recipes) do
    build_qualified(outcomes, recipes, Path.expand(Hypatia.Paths.verisimdb_data()))
  end

  defp build_qualified(outcomes, recipes, source) do
    # Qualify once before every graph consumer. Filtering only cross-repo
    # edges would still let unsupported observations affect direct edges and
    # trust normalization through extra nodes.
    outcomes =
      outcomes
      |> Enum.filter(fn outcome ->
        is_map(outcome) and OutcomeTracker.qualified_outcome?(outcome) and
          outcome_identities?(outcome)
      end)
      |> Enum.map(fn outcome -> Map.put(outcome, "recipe_id", outcome_recipe_id(outcome)) end)

    nodes = build_nodes(outcomes, recipes)
    edges = build_edges(outcomes)

    # Build cross-repo learning edges: repos that share successful
    # recipes are connected, weighted by shared success count
    cross_repo_edges = build_cross_repo_edges(outcomes)

    # Build language clusters: group repos by primary language family
    language_clusters = build_language_clusters(nodes, source)

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
    # Default trust = 0.5 (neutral)
    Map.get(scores, entity_id, 0.5)
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

  defp compute_trust(%__MODULE__{nodes: nodes} = graph) when map_size(nodes) == 0 do
    %{graph | last_computed: DateTime.utc_now()}
  end

  defp compute_trust(%__MODULE__{nodes: nodes, edges: edges} = graph) do
    # Cataloguing a recipe, or observing only failures, is not positive
    # evidence. PageRank's teleportation mass must not promote such nodes to
    # 1.0 through maximum-score normalization or perturb supported rankings.
    neutral_scores = Map.new(nodes, fn {id, _type} -> {id, 0.5} end)

    supported_edges =
      Enum.filter(edges, fn {source, target, weight} ->
        is_number(weight) and weight > 0 and Map.has_key?(nodes, source) and
          Map.has_key?(nodes, target)
      end)

    supported_nodes =
      supported_edges
      |> Enum.flat_map(fn {source, target, _weight} -> [source, target] end)
      |> MapSet.new()

    supported_scores = compute_supported_trust(supported_nodes, supported_edges)

    %{
      graph
      | trust_scores: Map.merge(neutral_scores, supported_scores),
        last_computed: DateTime.utc_now()
    }
  end

  defp compute_supported_trust(supported_nodes, edges) do
    n = MapSet.size(supported_nodes)

    # Only the qualified positive-support subgraph receives ranking mass.
    initial_score = 1.0 / max(n, 1)
    scores = Map.new(supported_nodes, fn id -> {id, initial_score} end)

    # Build adjacency: source -> [{target, weight}]. build_edges/1 emits one
    # edge per verified success, so the same evidence recorded twice arrives
    # as a parallel edge. Rank over the set of edges, as the cross-repo
    # (Jaccard) edges already do, so that trust depends on what was observed
    # and not on how many times it was logged.
    adjacency =
      edges
      |> Enum.uniq()
      |> Enum.reduce(%{}, fn {src, tgt, weight}, acc ->
        Map.update(acc, src, [{tgt, weight}], fn existing -> [{tgt, weight} | existing] end)
      end)

    # Iterative convergence
    if n == 0, do: %{}, else: iterate_trust(scores, adjacency, n, 0)
  end

  defp iterate_trust(scores, _adjacency, _n, iteration) when iteration >= @max_iterations do
    # Fall-through when convergence wasn't reached inside the iteration cap.
    # Normalise anyway so downstream consumers always see scores on the
    # same scale as the converged path.
    normalize_scores(scores)
  end

  defp iterate_trust(scores, adjacency, n, iteration) do
    # Pre-compute per-source outgoing-weight sums. PageRank requires a
    # column-stochastic transition matrix (each source's contributions
    # to its neighbours sum to at most the source's score). Dividing by
    # out-degree (count) only achieves this when all edges have equal
    # weight; with mixed weights we divide by the total weight instead
    # so the per-source contribution is conserved. See PROOFS-GAP.md
    # for why this premise matters for PageRank convergence.
    out_weight_sum =
      Map.new(adjacency, fn {src, targets} ->
        total = Enum.reduce(targets, 0.0, fn {_t, w}, acc -> acc + max(w, 0.0) end)
        {src, total}
      end)

    new_scores =
      Map.new(scores, fn {node_id, _old_score} ->
        incoming =
          Enum.reduce(adjacency, 0.0, fn {src, targets}, acc ->
            # Credit every edge src -> node_id, as `total` counts every edge
            # out of src; otherwise the column sums fall below 1 and trust
            # leaks. (Taking only the first edge lost most of a source's
            # trust while parallel edges reached this point.) Zero or
            # negative weights carry no trust: negative edges would break
            # the non-negative-matrix premise PageRank needs, and they are
            # filtered at source in `build_edges/1`.
            weight =
              Enum.reduce(targets, 0.0, fn
                {^node_id, w}, sum when is_number(w) and w > 0 -> sum + w
                _, sum -> sum
              end)

            total = Map.get(out_weight_sum, src, 0.0)

            if weight > 0 and total > 0 do
              acc + Map.get(scores, src, 0.0) * weight / total
            else
              acc
            end
          end)

        new_score = (1 - @damping_factor) / n + @damping_factor * incoming
        {node_id, new_score}
      end)

    # Check convergence
    max_delta =
      Enum.reduce(new_scores, 0.0, fn {id, new}, acc ->
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

  # Evidence must identify each actual graph subject. A qualified verdict
  # alone cannot authorize a fabricated "unknown" node or partial edge.
  # Canonical pattern is an explicit recipe identity, not a synthesized default.
  defp outcome_identities?(outcome) do
    Enum.all?(
      [Map.get(outcome, "repo"), Map.get(outcome, "bot"), outcome_recipe_id(outcome)],
      fn value -> is_binary(value) and String.trim(value) != "" end
    )
  end

  defp outcome_recipe_id(outcome),
    do: Map.get(outcome, "recipe_id") || Map.get(outcome, "pattern")

  defp build_nodes(outcomes, recipes) do
    repo_nodes = outcomes |> Enum.map(fn o -> {Map.fetch!(o, "repo"), :repo} end)
    bot_nodes = outcomes |> Enum.map(fn o -> {Map.fetch!(o, "bot"), :bot} end)
    recipe_nodes = recipes |> Enum.map(fn r -> {Map.get(r, "id"), :recipe} end)

    (repo_nodes ++ bot_nodes ++ recipe_nodes)
    |> Enum.reject(fn {id, _} -> is_nil(id) end)
    |> Map.new()
  end

  defp build_edges(outcomes) do
    # Only emit edges for successful outcomes. Negative edge weights
    # (failure = -0.5, false_positive = -1.0 in the previous
    # implementation) break the non-negative-matrix premise PageRank
    # needs to converge to a unique fixed point, and let the spectral
    # radius of the transition matrix climb above 1.
    #
    # Failure and false-positive signal is not lost — it is simply not
    # expressed by signing edge weights. The follow-up for the "penalty"
    # half of the signal is a post-processing anti-trust subtraction
    # from the converged scores; that change is out of scope for this
    # fix, which is purely about making the math correct. See
    # PROOFS-GAP.md — Section "Neural / PageRank".
    Enum.flat_map(outcomes, fn outcome ->
      case Map.get(outcome, "outcome") do
        "success" ->
          repo = Map.fetch!(outcome, "repo")
          bot = Map.fetch!(outcome, "bot")
          recipe = Map.fetch!(outcome, "recipe_id")

          [
            # recipe fixes repo
            {recipe, repo, 1.0},
            # bot executes recipe
            {bot, recipe, 1.0},
            # repo provides context to bot
            {repo, bot, 0.5}
          ]

        _ ->
          []
      end
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
      |> Enum.group_by(&Map.fetch!(&1, "repo"))
      |> Enum.map(fn {repo, repo_outcomes} ->
        recipes =
          repo_outcomes
          |> Enum.map(&Map.fetch!(&1, "recipe_id"))
          |> MapSet.new()

        {repo, recipes}
      end)

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
  defp build_language_clusters(nodes, source) do
    language_families = Hypatia.CrossRepoLearning.language_families()

    repos =
      nodes
      |> Enum.filter(fn {_id, type} -> type == :repo end)
      |> Enum.map(fn {id, _} -> id end)

    repos
    |> Enum.reduce(%{}, fn repo, clusters ->
      lang = detect_repo_language(repo, source)
      family = Map.get(language_families, lang, :unknown)

      if family != :unknown do
        Map.update(clusters, family, [repo], fn existing -> [repo | existing] end)
      else
        clusters
      end
    end)
  end

  # Detect primary language for a repo from scan data
  defp detect_repo_language(repo, source) do
    scan_path = Path.join([source, "scans", "#{repo}.json"])

    case File.read(scan_path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} ->
            # Deterministic resolution shared with CrossRepoLearning (#676)
            Hypatia.CrossRepoLearning.primary_language_from_scan_data(data)

          {:error, _} ->
            "unknown"
        end

      {:error, _} ->
        "unknown"
    end
  end

  defp load_all_outcomes(source) do
    outcomes_dir = Path.join(source, "outcomes")

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

  defp load_all_recipes(source) do
    recipes_dir = Path.join(source, "recipes")

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

            _ ->
              nil
          end
        end)
        |> Enum.reject(&is_nil/1)

      _ ->
        []
    end
  end
end
