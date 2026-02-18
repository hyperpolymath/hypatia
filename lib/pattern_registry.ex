# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.PatternRegistry do
  @moduledoc """
  Deduplicates findings into canonical patterns and tracks them across repos.

  Reads and writes to verisimdb-data/patterns/registry.json.
  Groups scan weak_points by (category, description_fingerprint),
  assigns pattern IDs, tracks occurrence counts, and detects trends.
  """

  require Logger

  @verisimdb_data_path "~/Documents/hyperpolymath-repos/verisimdb-data"

  @pa_rule_map %{
    "UncheckedAllocation" => "PA001",
    "UnboundedLoop" => "PA002",
    "BlockingIO" => "PA003",
    "UnsafeCode" => "PA004",
    "PanicPath" => "PA005",
    "RaceCondition" => "PA006",
    "DeadlockPotential" => "PA007",
    "ResourceLeak" => "PA008",
    "CommandInjection" => "PA009",
    "UnsafeDeserialization" => "PA010",
    "DynamicCodeExecution" => "PA011",
    "UnsafeFFI" => "PA012",
    "AtomExhaustion" => "PA013",
    "InsecureProtocol" => "PA014",
    "ExcessivePermissions" => "PA015",
    "PathTraversal" => "PA016",
    "HardcodedSecret" => "PA017",
    "UncheckedError" => "PA018",
    "InfiniteRecursion" => "PA019",
    "UnsafeTypeCoercion" => "PA020"
  }

  @doc """
  Build/update the pattern registry from scan data.

  Takes a list of scan results (from VerisimdbConnector.fetch_all_scans/0)
  and groups weak_points into canonical patterns tracked in registry.json.
  """
  def sync_from_scans(scans) do
    registry = load_registry()
    substitutions = load_substitutions()
    now = DateTime.utc_now() |> DateTime.to_iso8601()

    updated_registry =
      Enum.reduce(scans, registry, fn scan, reg ->
        weak_points = Map.get(scan.scan, "weak_points", [])

        Enum.reduce(weak_points, reg, fn wp, acc ->
          category = Map.get(wp, "category", "unknown")
          description = Map.get(wp, "description", category)
          severity = Map.get(wp, "severity", "Medium")
          repo = scan.repo

          pa_rule = Map.get(@pa_rule_map, category, "PA000")
          slug = fingerprint(description)
          pattern_id = "#{pa_rule}-#{slug}"

          tier = get_triangle_tier(category, substitutions)

          patterns = Map.get(acc, "patterns", %{})

          updated_pattern =
            case Map.get(patterns, pattern_id) do
              nil ->
                %{
                  "id" => pattern_id,
                  "category" => category,
                  "severity" => severity,
                  "description" => description,
                  "pa_rule" => pa_rule,
                  "occurrences" => 1,
                  "repos_affected" => 1,
                  "repos_affected_list" => [repo],
                  "first_seen" => now,
                  "last_seen" => now,
                  "trend" => "new",
                  "triangle_tier" => tier,
                  "recipe_id" => nil
                }

              existing ->
                repos_list = Map.get(existing, "repos_affected_list", [])

                {repos_list, repos_count} =
                  if repo in repos_list do
                    {repos_list, Map.get(existing, "repos_affected", 1)}
                  else
                    {repos_list ++ [repo], Map.get(existing, "repos_affected", 1) + 1}
                  end

                existing
                |> Map.put("occurrences", Map.get(existing, "occurrences", 0) + 1)
                |> Map.put("repos_affected", repos_count)
                |> Map.put("repos_affected_list", repos_list)
                |> Map.put("last_seen", now)
            end

          put_in(acc, ["patterns", pattern_id], updated_pattern)
        end)
      end)

    updated_registry = Map.put(updated_registry, "last_updated", now)
    save_registry(updated_registry)

    pattern_count = map_size(Map.get(updated_registry, "patterns", %{}))
    Logger.info("Pattern registry synced: #{pattern_count} patterns tracked")

    {:ok, updated_registry}
  end

  @doc "Get a single pattern by ID."
  def get_pattern(pattern_id) do
    registry = load_registry()
    Map.get(Map.get(registry, "patterns", %{}), pattern_id)
  end

  @doc "Get all patterns."
  def all_patterns do
    registry = load_registry()

    Map.get(registry, "patterns", %{})
    |> Map.values()
  end

  @doc "Filter patterns by category (PA rule number)."
  def patterns_by_category(category) do
    all_patterns()
    |> Enum.filter(fn p -> Map.get(p, "category") == category end)
  end

  @doc "Get patterns by trend direction: :increasing, :decreasing, :stable, :new"
  def trending_patterns(direction) do
    direction_str = Atom.to_string(direction)

    all_patterns()
    |> Enum.filter(fn p -> Map.get(p, "trend") == direction_str end)
  end

  @doc "Count total occurrences of a pattern across all repos."
  def pattern_occurrences(pattern_id) do
    case get_pattern(pattern_id) do
      nil -> 0
      pattern -> Map.get(pattern, "occurrences", 0)
    end
  end

  # --- Private ---

  defp registry_path do
    Path.join(Path.expand(@verisimdb_data_path), "patterns/registry.json")
  end

  defp substitutions_path do
    Path.join(Path.expand(@verisimdb_data_path), "recipes/proven-substitutions.json")
  end

  defp load_registry do
    case File.read(registry_path()) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} -> data
          {:error, _} -> %{"patterns" => %{}, "last_updated" => nil}
        end

      {:error, _} ->
        %{"patterns" => %{}, "last_updated" => nil}
    end
  end

  defp save_registry(registry) do
    case Jason.encode(registry, pretty: true) do
      {:ok, json} -> File.write!(registry_path(), json <> "\n")
      {:error, reason} -> Logger.error("Failed to save registry: #{inspect(reason)}")
    end
  end

  defp load_substitutions do
    case File.read(substitutions_path()) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} -> Map.get(data, "substitutions", [])
          {:error, _} -> []
        end

      {:error, _} ->
        []
    end
  end

  defp get_triangle_tier(category, substitutions) do
    case Enum.find(substitutions, fn s -> Map.get(s, "category") == category end) do
      nil -> "control"
      sub -> Map.get(sub, "triangle_tier", "control")
    end
  end

  defp fingerprint(description) when is_binary(description) do
    description
    |> String.downcase()
    |> String.replace(~r/[^a-z0-9]+/, "-")
    |> String.replace(~r/-+/, "-")
    |> String.trim("-")
    |> String.slice(0, 30)
  end

  defp fingerprint(_), do: "unknown"
end
