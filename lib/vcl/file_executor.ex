# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

defmodule Hypatia.VCL.FileExecutor do
  @moduledoc """
  VCL File Executor — executes parsed VCL ASTs against verisim-data flat files.

  This is the primary executor in Hypatia's federated data layer. It translates
  VCL queries into file operations against the verisim-data git-backed store,
  providing structured query semantics over flat JSON/JSONL files.

  ## Store Mapping

  VCL STORE names map to verisim-data directories:

  | Store Name  | Directory                    | Format |
  |-------------|------------------------------|--------|
  | scans       | verisim-data/scans/        | JSON   |
  | patterns    | verisim-data/patterns/     | JSON   |
  | recipes     | verisim-data/recipes/      | JSON   |
  | outcomes    | verisim-data/outcomes/     | JSONL  |
  | dispatch    | verisim-data/dispatch/     | JSONL  |
  | index       | verisim-data/index.json    | JSON   |

  ## Supported Modalities

  - DOCUMENT: Full JSON document contents
  - TEMPORAL: Ordered by timestamp, supports time-range filtering
  - GRAPH: Extracts relationships (repo→pattern, pattern→recipe, etc.)
  - SEMANTIC: Metadata and categorization
  """

  require Logger

  @verisimdb_data_path Application.compile_env(:hypatia, :verisimdb_data_path, "data/verisim")

  @store_map %{
    "scans" => "scans",
    "patterns" => "patterns",
    "recipes" => "recipes",
    "outcomes" => "outcomes",
    "dispatch" => "dispatch",
    "index" => ".",
    "verisim-data" => "."
  }

  # ---------------------------------------------------------------------------
  # Public API
  # ---------------------------------------------------------------------------

  @doc """
  Execute a parsed VCL AST against verisim-data files.

  Supports four source types:

  | Source                        | Description                                    |
  |-------------------------------|------------------------------------------------|
  | `{:store, store_id}`          | Query a named local store (scans, patterns, …) |
  | `{:federation, pattern, dp}`  | Cross-store federation with drift policy        |
  | `{:hexad, entity_id}`         | Single-entity lookup by ID                      |
  | `{:remote, url}`              | Clone/pull a remote verisim-data repo, query it |

  The `:remote` source uses `Hypatia.VCL.RemoteCache` to maintain a local
  git clone, then delegates to the standard store/federation logic with the
  cloned path as the data root.
  """
  def execute(ast, opts \\ []) do
    case ast.source do
      {:store, store_id} ->
        execute_store_query(store_id, ast, opts)

      {:federation, pattern, drift_policy} ->
        execute_federation_query(pattern, drift_policy, ast, opts)

      {:hexad, entity_id} ->
        execute_hexad_query(entity_id, ast, opts)

      {:remote, url} ->
        execute_remote_query(url, ast, opts)

      {:remote, url, sub_source} ->
        execute_remote_query(url, ast, opts, sub_source)
    end
  end

  # ---------------------------------------------------------------------------
  # Store Queries
  # ---------------------------------------------------------------------------

  defp execute_store_query(store_id, ast, _opts) do
    case Map.get(@store_map, store_id) do
      nil ->
        {:error, "Unknown store: #{store_id}"}

      dir ->
        base_path = Path.join(expand_path(), dir)

        results = case store_id do
          "scans" -> load_json_directory(base_path, ".json")
          "patterns" -> load_single_json(Path.join(base_path, "registry.json"))
          "recipes" -> load_json_directory(base_path, ".json", "recipe-")
          "outcomes" -> load_jsonl_directory(base_path)
          "dispatch" -> load_jsonl_directory(base_path)
          "index" -> load_single_json(Path.join(base_path, "index.json"))
          _ -> load_json_directory(base_path, ".json")
        end

        filtered = apply_where(results, ast.where)
        sorted = apply_modality_sort(filtered, ast.modalities)
        paginated = apply_pagination(sorted, ast.limit, ast.offset)

        {:ok, paginated}
    end
  end

  # ---------------------------------------------------------------------------
  # Federation Queries (cross-store)
  # ---------------------------------------------------------------------------

  defp execute_federation_query(pattern, drift_policy, ast, _opts) do
    # Federation patterns like /scans/*, /recipes/*, etc.
    store_name = pattern
      |> String.trim_leading("/")
      |> String.split("/")
      |> List.first()

    case Map.get(@store_map, store_name) do
      nil ->
        # Query across ALL stores
        results = Enum.flat_map(["scans", "patterns", "recipes", "outcomes"], fn store ->
          base_path = Path.join(expand_path(), store)
          case store do
            "scans" -> load_json_directory(base_path, ".json")
            "patterns" -> load_single_json(Path.join(expand_path(), "patterns/registry.json"))
            "recipes" -> load_json_directory(base_path, ".json", "recipe-")
            "outcomes" -> load_jsonl_directory(base_path)
            _ -> []
          end
        end)

        filtered = apply_where(results, ast.where)
        drifted = apply_drift_policy(filtered, drift_policy)
        paginated = apply_pagination(drifted, ast.limit, ast.offset)
        {:ok, paginated}

      dir ->
        base_path = Path.join(expand_path(), dir)

        results = cond do
          store_name in ["outcomes", "dispatch"] -> load_jsonl_directory(base_path)
          store_name == "patterns" -> load_single_json(Path.join(base_path, "registry.json"))
          true -> load_json_directory(base_path, ".json")
        end

        filtered = apply_where(results, ast.where)
        drifted = apply_drift_policy(filtered, drift_policy)
        paginated = apply_pagination(drifted, ast.limit, ast.offset)
        {:ok, paginated}
    end
  end

  # ---------------------------------------------------------------------------
  # Remote Queries (multi-store federation via git clone cache)
  # ---------------------------------------------------------------------------

  # Execute a query against a remote verisim-data repository. The remote
  # repo is cloned (or refreshed) via RemoteCache, then queried using the
  # same store/federation logic as local data. If `sub_source` is provided
  # it specifies what to query within the remote clone; otherwise the query
  # defaults to a full federation across all stores in the clone.
  defp execute_remote_query(url, ast, opts, sub_source \\ nil) do
    cache_opts = Keyword.get(opts, :cache_opts, [])

    case Hypatia.VCL.RemoteCache.cache_remote_store(url, cache_opts) do
      {:ok, local_path} ->
        # Build a new AST that targets the appropriate source within the clone.
        remote_ast =
          case sub_source do
            {:store, store_id} ->
              %{ast | source: {:store, store_id}}

            {:federation, pattern, drift_policy} ->
              %{ast | source: {:federation, pattern, drift_policy}}

            nil ->
              # Default: federate across all stores in the remote clone.
              %{ast | source: {:federation, "*", nil}}
          end

        # Temporarily override the data path to point at the remote clone.
        # We pass the override through opts so that path-resolution helpers
        # can pick it up without mutating module-level state.
        remote_opts = Keyword.put(opts, :data_path_override, local_path)
        execute_with_path(remote_ast, remote_opts)

      {:error, reason} ->
        {:error, "Remote cache failed for #{url}: #{reason}"}
    end
  end

  # Execute a query with an explicit data path override. Dispatches to the
  # same store/federation/hexad logic but resolves paths against the override
  # directory instead of the default @verisimdb_data_path.
  defp execute_with_path(ast, opts) do
    case ast.source do
      {:store, store_id} ->
        execute_store_query_at(store_id, ast, opts)

      {:federation, pattern, drift_policy} ->
        execute_federation_query_at(pattern, drift_policy, ast, opts)

      {:hexad, entity_id} ->
        execute_hexad_query(entity_id, ast, opts)
    end
  end

  # Store query against an arbitrary base path (used for remote clones).
  defp execute_store_query_at(store_id, ast, opts) do
    base = Keyword.get(opts, :data_path_override, expand_path())

    case Map.get(@store_map, store_id) do
      nil ->
        {:error, "Unknown store: #{store_id}"}

      dir ->
        base_path = Path.join(base, dir)

        results =
          case store_id do
            "scans" -> load_json_directory(base_path, ".json")
            "patterns" -> load_single_json(Path.join(base_path, "registry.json"))
            "recipes" -> load_json_directory(base_path, ".json", "recipe-")
            "outcomes" -> load_jsonl_directory(base_path)
            "dispatch" -> load_jsonl_directory(base_path)
            "index" -> load_single_json(Path.join(base_path, "index.json"))
            _ -> load_json_directory(base_path, ".json")
          end

        filtered = apply_where(results, ast.where)
        sorted = apply_modality_sort(filtered, ast.modalities)
        paginated = apply_pagination(sorted, ast.limit, ast.offset)

        {:ok, paginated}
    end
  end

  # Federation query against an arbitrary base path (used for remote clones).
  defp execute_federation_query_at(pattern, drift_policy, ast, opts) do
    base = Keyword.get(opts, :data_path_override, expand_path())

    store_name =
      pattern
      |> String.trim_leading("/")
      |> String.split("/")
      |> List.first()

    case Map.get(@store_map, store_name) do
      nil ->
        # Query across ALL stores in the remote clone.
        results =
          Enum.flat_map(["scans", "patterns", "recipes", "outcomes"], fn store ->
            base_path = Path.join(base, store)

            case store do
              "scans" -> load_json_directory(base_path, ".json")
              "patterns" -> load_single_json(Path.join(base, "patterns/registry.json"))
              "recipes" -> load_json_directory(base_path, ".json", "recipe-")
              "outcomes" -> load_jsonl_directory(base_path)
              _ -> []
            end
          end)

        filtered = apply_where(results, ast.where)
        drifted = apply_drift_policy(filtered, drift_policy)
        paginated = apply_pagination(drifted, ast.limit, ast.offset)
        {:ok, paginated}

      dir ->
        base_path = Path.join(base, dir)

        results =
          cond do
            store_name in ["outcomes", "dispatch"] -> load_jsonl_directory(base_path)
            store_name == "patterns" -> load_single_json(Path.join(base_path, "registry.json"))
            true -> load_json_directory(base_path, ".json")
          end

        filtered = apply_where(results, ast.where)
        drifted = apply_drift_policy(filtered, drift_policy)
        paginated = apply_pagination(drifted, ast.limit, ast.offset)
        {:ok, paginated}
    end
  end

  # ---------------------------------------------------------------------------
  # Hexad Queries (single entity by ID)
  # ---------------------------------------------------------------------------

  defp execute_hexad_query(entity_id, ast, _opts) do
    # Entity ID could be a repo name, pattern ID, or recipe ID
    result = cond do
      # Try as scan (repo name)
      File.exists?(scan_path(entity_id)) ->
        load_single_json(scan_path(entity_id))

      # Try as recipe
      File.exists?(recipe_path(entity_id)) ->
        load_single_json(recipe_path(entity_id))

      # Try as pattern ID in registry
      true ->
        case load_single_json(Path.join(expand_path(), "patterns/registry.json")) do
          [%{"patterns" => patterns}] ->
            case Map.get(patterns, entity_id) do
              nil -> []
              pattern -> [pattern]
            end
          _ -> []
        end
    end

    filtered = apply_where(result, ast.where)
    {:ok, filtered}
  end

  # ---------------------------------------------------------------------------
  # WHERE Clause Filtering
  # ---------------------------------------------------------------------------

  defp apply_where(results, nil), do: results

  defp apply_where(results, {:field, field, op, value}) do
    Enum.filter(results, fn item ->
      evaluate_field_condition(item, field, op, value)
    end)
  end

  defp apply_where(results, {:fulltext, :contains, text}) do
    text_lower = String.downcase(text)
    Enum.filter(results, fn item ->
      item_text = item |> Jason.encode!() |> String.downcase()
      String.contains?(item_text, text_lower)
    end)
  end

  defp apply_where(results, {:fulltext, :matches, pattern}) do
    case Regex.compile(pattern, "i") do
      {:ok, regex} ->
        Enum.filter(results, fn item ->
          item_text = Jason.encode!(item)
          Regex.match?(regex, item_text)
        end)

      {:error, _} ->
        apply_where(results, {:fulltext, :contains, pattern})
    end
  end

  defp apply_where(results, {:and, conditions}) do
    Enum.reduce(conditions, results, fn condition, acc ->
      apply_where(acc, condition)
    end)
  end

  defp evaluate_field_condition(item, field, op, value) do
    item_value = deep_get(item, field)

    case op do
      :eq -> to_string(item_value) == value
      :neq -> to_string(item_value) != value
      :gt -> compare_values(item_value, value) == :gt
      :gte -> compare_values(item_value, value) in [:gt, :eq]
      :lt -> compare_values(item_value, value) == :lt
      :lte -> compare_values(item_value, value) in [:lt, :eq]
      :contains ->
        cond do
          is_list(item_value) -> value in Enum.map(item_value, &to_string/1)
          is_binary(item_value) -> String.contains?(item_value, value)
          true -> false
        end
      :like ->
        is_binary(item_value) and
          String.contains?(String.downcase(item_value), String.downcase(value))
      :matches ->
        case Regex.compile(value, "i") do
          {:ok, regex} -> is_binary(item_value) and Regex.match?(regex, item_value)
          _ -> false
        end
      {:raw, _} -> false
    end
  end

  defp deep_get(item, field) when is_map(item) do
    # Support dotted paths like "triangle_status.eliminate"
    case String.split(field, ".", parts: 2) do
      [key] -> Map.get(item, key)
      [key, rest] ->
        case Map.get(item, key) do
          child when is_map(child) -> deep_get(child, rest)
          _ -> nil
        end
    end
  end

  defp deep_get(_, _), do: nil

  defp compare_values(a, b) when is_number(a) do
    case Float.parse(b) do
      {b_num, _} -> cond do
        a > b_num -> :gt
        a < b_num -> :lt
        true -> :eq
      end
      :error -> :lt
    end
  end

  defp compare_values(a, b) when is_binary(a) do
    cond do
      a > b -> :gt
      a < b -> :lt
      true -> :eq
    end
  end

  defp compare_values(_, _), do: :lt

  # ---------------------------------------------------------------------------
  # Drift Policy Application
  # ---------------------------------------------------------------------------

  # Apply drift policies to cross-repo federation query results.
  # Drift policies control how cross-repo confidence data is weighted
  # when queried through VCL FEDERATION queries.
  #
  # | Policy          | Effect                                              |
  # |-----------------|-----------------------------------------------------|
  # | nil / ""        | No modification (pass-through)                      |
  # | "conservative"  | Discount confidence by 50%                          |
  # | "moderate"      | Discount confidence by 25%                          |
  # | "aggressive"    | No discount (trust cross-repo equally)              |
  # | "language_aware"| Full within language family, 70% discount otherwise |

  defp apply_drift_policy(results, nil), do: results
  defp apply_drift_policy(results, ""), do: results

  defp apply_drift_policy(results, policy_str) when is_binary(policy_str) do
    policy = parse_drift_policy(policy_str)

    case policy do
      :aggressive ->
        # No modification — trust cross-repo data equally
        results

      :conservative ->
        apply_confidence_discount(results, 0.5)

      :moderate ->
        apply_confidence_discount(results, 0.75)

      :language_aware ->
        apply_language_aware_discount(results)

      _ ->
        results
    end
  end

  defp apply_drift_policy(results, _), do: results

  defp parse_drift_policy(str) do
    case String.downcase(String.trim(str)) do
      "conservative" -> :conservative
      "moderate" -> :moderate
      "aggressive" -> :aggressive
      "language_aware" -> :language_aware
      "language-aware" -> :language_aware
      _ -> :moderate
    end
  end

  # Apply a flat discount to any confidence values in the results.
  # Looks for "confidence" keys at any level of the result map.
  defp apply_confidence_discount(results, factor) do
    Enum.map(results, fn item ->
      discount_confidence_in_map(item, factor)
    end)
  end

  defp discount_confidence_in_map(item, factor) when is_map(item) do
    Enum.reduce(item, %{}, fn {key, value}, acc ->
      new_value =
        cond do
          key in ["confidence", "confidence_raw"] and is_number(value) ->
            Float.round(value * factor, 4)

          is_map(value) ->
            discount_confidence_in_map(value, factor)

          true ->
            value
        end

      Map.put(acc, key, new_value)
    end)
  end

  defp discount_confidence_in_map(item, _factor), do: item

  # For language-aware policy, group results by repo language family
  # and apply full confidence within-family, discounted cross-family.
  # Results without a detectable repo get moderate discount.
  defp apply_language_aware_discount(results) do
    language_families = Hypatia.CrossRepoLearning.language_families()

    Enum.map(results, fn item ->
      repo = Map.get(item, "repo", "")
      lang = detect_item_language(item, repo)
      family = Map.get(language_families, lang, :unknown)

      # Tag each result with its language family for downstream use
      item
      |> Map.put("_language_family", Atom.to_string(family))
      |> Map.put("_drift_discount",
        if(family == :unknown, do: 0.75, else: 1.0))
    end)
  end

  # Try to determine language from the item itself or from scan data
  defp detect_item_language(item, repo) do
    cond do
      Map.has_key?(item, "primary_language") ->
        String.downcase(Map.get(item, "primary_language", "unknown"))

      repo != "" ->
        Hypatia.CrossRepoLearning.drift_discount(:language_aware, repo, repo)
        # We just need the language, not the discount — detect via scan
        scan_path = Path.join([expand_path(), "scans", "#{repo}.json"])
        case File.read(scan_path) do
          {:ok, content} ->
            case Jason.decode(content) do
              {:ok, data} ->
                String.downcase(Map.get(data, "primary_language", "unknown"))
              _ -> "unknown"
            end
          _ -> "unknown"
        end

      true -> "unknown"
    end
  end

  # ---------------------------------------------------------------------------
  # Modality-Based Sorting
  # ---------------------------------------------------------------------------

  defp apply_modality_sort(results, modalities) do
    cond do
      :temporal in modalities ->
        Enum.sort_by(results, fn item ->
          Map.get(item, "timestamp") || Map.get(item, "last_seen") ||
            Map.get(item, "last_scan") || ""
        end, :desc)

      :semantic in modalities ->
        Enum.sort_by(results, fn item ->
          Map.get(item, "category") || ""
        end)

      true ->
        results
    end
  end

  # ---------------------------------------------------------------------------
  # Pagination
  # ---------------------------------------------------------------------------

  defp apply_pagination(results, nil, nil), do: results
  defp apply_pagination(results, limit, nil) when is_integer(limit), do: Enum.take(results, limit)
  defp apply_pagination(results, nil, offset) when is_integer(offset), do: Enum.drop(results, offset)
  defp apply_pagination(results, limit, offset) do
    results |> Enum.drop(offset || 0) |> Enum.take(limit)
  end

  # ---------------------------------------------------------------------------
  # File Loading
  # ---------------------------------------------------------------------------

  defp load_json_directory(path, extension, prefix \\ nil) do
    case File.ls(path) do
      {:ok, files} ->
        files
        |> Enum.filter(fn f ->
          ends_ok = String.ends_with?(f, extension)
          prefix_ok = if prefix, do: String.starts_with?(f, prefix), else: true
          ends_ok and prefix_ok
        end)
        |> Enum.map(fn f ->
          full_path = Path.join(path, f)
          case File.read(full_path) do
            {:ok, content} ->
              case Jason.decode(content) do
                {:ok, data} ->
                  # Tag with source filename
                  if is_map(data) do
                    Map.put(data, "_source", f)
                  else
                    %{"_source" => f, "_data" => data}
                  end
                {:error, _} -> nil
              end
            {:error, _} -> nil
          end
        end)
        |> Enum.reject(&is_nil/1)

      {:error, reason} ->
        Logger.warning("VCL FileExecutor: cannot read #{path}: #{inspect(reason)}")
        []
    end
  end

  defp load_single_json(path) do
    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} when is_map(data) -> [data]
          {:ok, data} when is_list(data) -> data
          {:error, _} -> []
        end

      {:error, _} ->
        []
    end
  end

  defp load_jsonl_directory(path) do
    case File.ls(path) do
      {:ok, files} ->
        # Stream JSONL files line-by-line to avoid loading entire files
        # into memory. This prevents timeout cascades on large datasets
        # (e.g. 6,900+ outcome records).
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.flat_map(fn f ->
          full_path = Path.join(path, f)

          full_path
          |> File.stream!()
          |> Stream.map(fn line ->
            case Jason.decode(String.trim(line)) do
              {:ok, record} -> record
              {:error, _} -> nil
            end
          end)
          |> Stream.reject(&is_nil/1)
          |> Enum.to_list()
        end)

      {:error, _} ->
        []
    end
  end

  # ---------------------------------------------------------------------------
  # Path Helpers
  # ---------------------------------------------------------------------------

  defp expand_path, do: Path.expand(@verisimdb_data_path)

  defp scan_path(repo_name) do
    Path.join([expand_path(), "scans", "#{repo_name}.json"])
  end

  defp recipe_path(recipe_id) do
    Path.join([expand_path(), "recipes", "#{recipe_id}.json"])
  end
end
