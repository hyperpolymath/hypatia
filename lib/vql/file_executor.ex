# SPDX-License-Identifier: PMPL-1.0-or-later
# Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <jonathan.jewell@open.ac.uk>

defmodule Hypatia.VQL.FileExecutor do
  @moduledoc """
  VQL File Executor — executes parsed VQL ASTs against verisimdb-data flat files.

  This is the primary executor in Hypatia's federated data layer. It translates
  VQL queries into file operations against the verisimdb-data git-backed store,
  providing structured query semantics over flat JSON/JSONL files.

  ## Store Mapping

  VQL STORE names map to verisimdb-data directories:

  | Store Name  | Directory                    | Format |
  |-------------|------------------------------|--------|
  | scans       | verisimdb-data/scans/        | JSON   |
  | patterns    | verisimdb-data/patterns/     | JSON   |
  | recipes     | verisimdb-data/recipes/      | JSON   |
  | outcomes    | verisimdb-data/outcomes/     | JSONL  |
  | dispatch    | verisimdb-data/dispatch/     | JSONL  |
  | index       | verisimdb-data/index.json    | JSON   |

  ## Supported Modalities

  - DOCUMENT: Full JSON document contents
  - TEMPORAL: Ordered by timestamp, supports time-range filtering
  - GRAPH: Extracts relationships (repo→pattern, pattern→recipe, etc.)
  - SEMANTIC: Metadata and categorization
  """

  require Logger

  @verisimdb_data_path "~/Documents/hyperpolymath-repos/verisimdb-data"

  @store_map %{
    "scans" => "scans",
    "patterns" => "patterns",
    "recipes" => "recipes",
    "outcomes" => "outcomes",
    "dispatch" => "dispatch",
    "index" => ".",
    "verisimdb-data" => "."
  }

  # ---------------------------------------------------------------------------
  # Public API
  # ---------------------------------------------------------------------------

  @doc "Execute a parsed VQL AST against verisimdb-data files."
  def execute(ast, opts \\ []) do
    case ast.source do
      {:store, store_id} ->
        execute_store_query(store_id, ast, opts)

      {:federation, pattern, drift_policy} ->
        execute_federation_query(pattern, drift_policy, ast, opts)

      {:hexad, entity_id} ->
        execute_hexad_query(entity_id, ast, opts)
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

  defp execute_federation_query(pattern, _drift_policy, ast, _opts) do
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
        paginated = apply_pagination(filtered, ast.limit, ast.offset)
        {:ok, paginated}

      dir ->
        base_path = Path.join(expand_path(), dir)

        results = cond do
          store_name in ["outcomes", "dispatch"] -> load_jsonl_directory(base_path)
          store_name == "patterns" -> load_single_json(Path.join(base_path, "registry.json"))
          true -> load_json_directory(base_path, ".json")
        end

        filtered = apply_where(results, ast.where)
        paginated = apply_pagination(filtered, ast.limit, ast.offset)
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
        Logger.warning("VQL FileExecutor: cannot read #{path}: #{inspect(reason)}")
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
        files
        |> Enum.filter(&String.ends_with?(&1, ".jsonl"))
        |> Enum.flat_map(fn f ->
          full_path = Path.join(path, f)
          case File.read(full_path) do
            {:ok, content} ->
              content
              |> String.split("\n", trim: true)
              |> Enum.map(fn line ->
                case Jason.decode(line) do
                  {:ok, record} -> record
                  {:error, _} -> nil
                end
              end)
              |> Enum.reject(&is_nil/1)

            {:error, _} -> []
          end
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
