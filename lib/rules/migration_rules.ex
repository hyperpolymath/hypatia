# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.Rules.MigrationRules do
  @moduledoc """
  Primary Elixir ReScript migration rules (migrated from legacy Logtalk engine).

  Tracks deprecated API usage, migration readiness, and merge conflict resolution.
  """

  # ---------------------------------------------------------------------------
  # Deprecated API Replacements (ReScript v12→v13)
  # ---------------------------------------------------------------------------

  @deprecated_apis %{
    "Js.Array2" => %{replacement: "Array", severity: :high, strategy: :search_replace},
    "Js.String2" => %{replacement: "String", severity: :high, strategy: :search_replace},
    "Js.Dict" => %{replacement: "Dict", severity: :high, strategy: :module_replace},
    "Belt.Array" => %{replacement: "Array", severity: :high, strategy: :module_replace},
    "Belt.List" => %{replacement: "List", severity: :high, strategy: :module_replace},
    "Belt.Map" => %{replacement: "Map", severity: :medium, strategy: :module_replace},
    "Belt.Set" => %{replacement: "Set", severity: :medium, strategy: :module_replace},
    "Belt.Option" => %{replacement: "Option", severity: :medium, strategy: :module_replace},
    "Belt.Result" => %{replacement: "Result", severity: :medium, strategy: :module_replace},
    "Js.Promise" => %{replacement: "Promise", severity: :medium, strategy: :module_replace},
    "Js.Nullable" => %{replacement: "Nullable", severity: :medium, strategy: :module_replace},
    "Js.Json" => %{replacement: "JSON", severity: :medium, strategy: :module_replace},
    "Js.Console" => %{replacement: "Console", severity: :low, strategy: :module_replace},
    "Js.log" => %{replacement: "Console.log", severity: :low, strategy: :search_replace},
    "Js.Float" => %{replacement: "Float", severity: :low, strategy: :module_replace},
    "Js.Int" => %{replacement: "Int", severity: :low, strategy: :module_replace},
    "Js.Math" => %{replacement: "Math", severity: :low, strategy: :module_replace},
    "Js.Re" => %{replacement: "RegExp", severity: :low, strategy: :module_replace}
  }

  def deprecated_apis, do: @deprecated_apis

  def scan_deprecated_usage(content) do
    Enum.flat_map(@deprecated_apis, fn {api, info} ->
      count = length(Regex.scan(~r/#{Regex.escape(api)}/, content))
      if count > 0 do
        [%{api: api, replacement: info.replacement, severity: info.severity,
           strategy: info.strategy, count: count}]
      else
        []
      end
    end)
  end

  # ---------------------------------------------------------------------------
  # Migration Readiness
  # ---------------------------------------------------------------------------

  def migration_ready?(repo_info) do
    health = Map.get(repo_info, :health, 0)
    conflicts = Map.get(repo_info, :merge_conflicts, 0)
    build_passing = Map.get(repo_info, :build_passing, false)

    health >= 0.8 and conflicts == 0 and build_passing
  end

  def migration_blocked?(repo_info) do
    cond do
      Map.get(repo_info, :merge_conflicts, 0) > 0 -> {:blocked, :merge_conflicts}
      not Map.get(repo_info, :build_passing, false) -> {:blocked, :build_failing}
      Map.get(repo_info, :deprecated_count, 0) > 100 -> {:blocked, :heavy_deprecated_usage}
      true -> :not_blocked
    end
  end

  def migration_priority(repo_info) do
    health = Map.get(repo_info, :health, 0)
    dep_count = Map.get(repo_info, :dep_count, 0)
    deprecated = Map.get(repo_info, :deprecated_count, 0)

    health * 100 - dep_count * 5 - deprecated * 0.1
  end

  # ---------------------------------------------------------------------------
  # Merge Conflict Resolution
  # ---------------------------------------------------------------------------

  @modern_api_prefixes ["Array.", "Dict.", "Console.", "String.", "Promise.", "Option.",
                         "Result.", "Map.", "Set.", "Int.", "Float.", "Math.", "RegExp.",
                         "Nullable.", "JSON."]

  @deprecated_api_prefixes ["Js.", "Belt."]

  def uses_modern_api?(code) do
    Enum.any?(@modern_api_prefixes, &String.contains?(code, &1))
  end

  def uses_deprecated_api?(code) do
    Enum.any?(@deprecated_api_prefixes, &String.contains?(code, &1))
  end

  def resolve_conflict(ours, theirs) do
    cond do
      uses_modern_api?(ours) and uses_deprecated_api?(theirs) ->
        {:chose_ours, 0.95}
      uses_modern_api?(theirs) and uses_deprecated_api?(ours) ->
        {:chose_theirs, 0.95}
      true ->
        {:manual_merge, 0.5}
    end
  end
end
