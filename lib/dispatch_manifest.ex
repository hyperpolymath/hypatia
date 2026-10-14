# SPDX-License-Identifier: MPL-2.0

defmodule Hypatia.DispatchManifest do
  @moduledoc """
  Writes dispatch manifests (JSONL files) from routed actions.

  The manifest is the bridge between hypatia's decision layer (Elixir)
  and the execution layer (robot-repo-automaton CLI + fleet fix scripts).

  Each line is a JSON object describing one action to execute:
  - tier: "eliminate" | "substitute" | "control"
  - strategy: "auto_execute" | "review" | "report_only"
  - recipe_id, confidence, fix_script (for eliminate)
  - proven_module (for substitute)
  - pattern_id, category, description, repo, severity
  - facts: the verdict records the step derives from (HYP-01)

  Facts derive workflows, never the reverse (HYP-01). Every step names the
  fact it was derived from: the pattern it acts on, that pattern's support
  (`observed` with the fleet occurrence count, or `pattern`) and the level
  derived from them (`Hypatia.Verdict.level/2`). A step that can name no
  fact is refused, and so is the whole manifest: `compile/1` returns
  `{:error, {:factless_steps, steps}}` and `write/2` writes nothing, so
  dispatch-runner.sh never sees a step that rests on nothing. A step's
  Bayesian `confidence` is belief and stays in its own field; it is never a
  fact.
  """

  alias Hypatia.TriangleRouter
  alias Hypatia.Verdict

  require Logger

  @verisimdb_path Application.compile_env(:hypatia, :verisimdb_data_path, "data/verisim")
  @fix_scripts_dir Application.compile_env(:hypatia, :fix_scripts_dir, "scripts/fix-scripts")

  @doc """
  Compile routed actions into manifest steps, each naming its facts.

  Returns `{:ok, steps}`, or `{:error, {:factless_steps, steps}}` naming
  every step (its position, tier and pattern id) that could name no fact.
  """
  @spec compile(list()) :: {:ok, [map()]} | {:error, {:factless_steps, [map()]}}
  def compile(routed_actions) do
    lines =
      Enum.flat_map(routed_actions, fn action ->
        case action_to_manifest_entry(action) do
          nil -> []
          entries when is_list(entries) -> entries
          entry -> [entry]
        end
      end)

    factless =
      lines
      |> Enum.with_index()
      |> Enum.filter(fn {entry, _} -> entry["facts"] == [] end)
      |> Enum.map(fn {entry, index} ->
        %{"step" => index, "tier" => entry["tier"], "pattern_id" => entry["pattern_id"]}
      end)

    case factless do
      [] -> {:ok, lines}
      steps -> {:error, {:factless_steps, steps}}
    end
  end

  @doc """
  Write a dispatch manifest from a list of routed actions.

  Returns `{:ok, path, stats}`, or `{:error, {:factless_steps, steps}}`
  without writing anything when a step names no fact (`compile/1`).
  `opts[:dispatch_dir]` overrides the directory (default
  `<verisimdb_data_path>/dispatch`).
  """
  def write(routed_actions, opts \\ []) do
    case compile(routed_actions) do
      {:ok, lines} ->
        write_lines(
          lines,
          Keyword.get(opts, :dispatch_dir, Path.join(@verisimdb_path, "dispatch"))
        )

      {:error, {:factless_steps, steps}} = refused ->
        Logger.error(
          "Dispatch manifest REFUSED: #{length(steps)} step(s) name no fact " <>
            "(#{inspect(Enum.take(steps, 5))}); nothing written"
        )

        refused
    end
  end

  defp write_lines(lines, dispatch_dir) do
    File.mkdir_p!(dispatch_dir)

    manifest_path = Path.join(dispatch_dir, "pending.jsonl")

    # Also write a timestamped archive copy
    archive_name = "dispatch-#{Date.utc_today() |> Date.to_iso8601()}.jsonl"
    archive_path = Path.join(dispatch_dir, archive_name)

    content =
      Enum.map_join(lines, "\n", fn entry ->
        {:ok, json} = Jason.encode(entry)
        json
      end)

    # Add trailing newline if content is not empty
    content = if content != "", do: content <> "\n", else: ""

    File.write!(manifest_path, content)
    File.write!(archive_path, content)

    stats = %{
      total: length(lines),
      auto_execute: Enum.count(lines, &(&1["strategy"] == "auto_execute")),
      review: Enum.count(lines, &(&1["strategy"] == "review")),
      report_only: Enum.count(lines, &(&1["strategy"] == "report_only")),
      by_tier: %{
        eliminate: Enum.count(lines, &(&1["tier"] == "eliminate")),
        substitute: Enum.count(lines, &(&1["tier"] == "substitute")),
        control: Enum.count(lines, &(&1["tier"] == "control"))
      }
    }

    Logger.info(
      "Dispatch manifest written: #{stats.total} actions " <>
        "(#{stats.auto_execute} auto, #{stats.review} review, #{stats.report_only} report) " <>
        "to #{manifest_path}"
    )

    {:ok, manifest_path, stats}
  end

  # --- Private ---

  defp action_to_manifest_entry({:eliminate, recipe, pattern}) do
    confidence = Map.get(recipe, "confidence", 0.0)
    strategy = TriangleRouter.dispatch_strategy(confidence)
    repo = get_repo(pattern)

    %{
      "tier" => "eliminate",
      "strategy" => Atom.to_string(strategy),
      "pattern_id" => Map.get(pattern, "id", "unknown"),
      "category" => Map.get(pattern, "category", "unknown"),
      "description" => Map.get(pattern, "description", ""),
      "severity" => Map.get(pattern, "severity", "Medium"),
      "repo" => repo,
      "program_path" => resolve_program_path(pattern, repo),
      "recipe_id" => Map.get(recipe, "id"),
      "confidence" => confidence,
      "auto_fixable" => Map.get(recipe, "auto_fixable", false),
      "fix_script" => resolve_fix_script(Map.get(recipe, "fix_script")),
      "action" => Map.get(recipe, "action", "unknown"),
      "match" => Map.get(recipe, "match"),
      "replacement" => Map.get(recipe, "replacement"),
      "proven_module" => Map.get(recipe, "proven_module"),
      "facts" => facts_for(pattern),
      "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601()
    }
  end

  defp action_to_manifest_entry({:substitute, recipe, pattern}) do
    confidence = Map.get(recipe, "confidence", 0.0)
    repo = get_repo(pattern)

    %{
      "tier" => "substitute",
      "strategy" => "review",
      "pattern_id" => Map.get(pattern, "id", "unknown"),
      "category" => Map.get(pattern, "category", "unknown"),
      "description" => Map.get(pattern, "description", ""),
      "severity" => Map.get(pattern, "severity", "Medium"),
      "repo" => repo,
      "program_path" => resolve_program_path(pattern, repo),
      "recipe_id" => Map.get(recipe, "id"),
      "confidence" => confidence,
      "auto_fixable" => false,
      "proven_module" => Map.get(recipe, "proven_module"),
      "proven_modules" => Map.get(recipe, "proven_modules", []),
      "formally_proven" => Map.get(recipe, "formally_proven", false),
      "facts" => facts_for(pattern),
      "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601()
    }
  end

  defp action_to_manifest_entry({:control, finding}) do
    repo = get_repo(finding)

    %{
      "tier" => "control",
      "strategy" => "report_only",
      "pattern_id" => Map.get(finding, "id", "unknown"),
      "category" => Map.get(finding, "category", "unknown"),
      "description" => Map.get(finding, "description", ""),
      "severity" => Map.get(finding, "severity", "Medium"),
      "repo" => repo,
      "program_path" => resolve_program_path(finding, repo),
      "recipe_id" => nil,
      "confidence" => 0.0,
      "auto_fixable" => false,
      "facts" => facts_for(finding),
      "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601()
    }
  end

  defp action_to_manifest_entry(_), do: nil

  # The verdict a step derives from. A registry pattern is evidence that a
  # pattern matched, `occurrences` times across the fleet: `observed` support
  # at level epi. Without a pattern id there is nothing to name, and the step
  # is factless (compile/1 refuses the manifest).
  defp facts_for(source) do
    case Map.get(source, "id") do
      id when is_binary(id) and id not in ["", "unknown"] ->
        {support, observations} =
          case Map.get(source, "occurrences") do
            n when is_integer(n) and n > 0 -> {%{kind: "observed", observations: n}, n}
            _ -> {%{kind: "pattern"}, 1}
          end

        evidence = [
          %{
            rule: "pattern_registry/#{id}",
            rule_version: Verdict.rule_version(),
            observations: observations
          }
        ]

        [
          %{
            "pattern_id" => id,
            "support" => Map.new(support, fn {k, v} -> {Atom.to_string(k), v} end),
            "evidence" =>
              Enum.map(evidence, &Map.new(&1, fn {k, v} -> {Atom.to_string(k), v} end)),
            "level" => Verdict.level(support, evidence)
          }
        ]

      _ ->
        []
    end
  end

  defp get_repo(pattern_or_finding) do
    Map.get(pattern_or_finding, "routed_repo") ||
      case Map.get(pattern_or_finding, "repos_affected_list", []) do
        [r | _] -> r
        _ -> "unknown"
      end
  end

  defp resolve_program_path(pattern_or_finding, repo) do
    # Try repo_paths map first (from PatternRegistry), fall back to repo-paths.json, then standard
    path =
      case Map.get(pattern_or_finding, "repo_paths", %{}) do
        paths when is_map(paths) and map_size(paths) > 0 ->
          Map.get(paths, repo)

        _ ->
          nil
      end

    cond do
      is_binary(path) and path != "" and path != "." -> path
      true -> resolve_from_index(repo)
    end
  end

  @doc false
  defp resolve_fix_script(nil), do: nil
  defp resolve_fix_script(""), do: nil

  defp resolve_fix_script(script_name) do
    # Resolve bare script names (e.g. "fix-unpinned-actions.sh") to full paths
    # under the fix-scripts directory. Already-absolute paths pass through.
    cond do
      String.starts_with?(script_name, "/") ->
        script_name

      true ->
        full_path = Path.join(@fix_scripts_dir, script_name)

        if File.exists?(full_path) do
          Path.expand(full_path)
        else
          Logger.warning("Fix script not found: #{full_path} -- passing through raw name")
          script_name
        end
    end
  end

  @repo_paths_file Path.join(@verisimdb_path, "repo-paths.json")

  defp resolve_from_index(repo) do
    repos_dir = System.get_env("HYPATIA_REPOS_DIR", File.cwd!())
    default_path = Path.join(repos_dir, repo)

    case File.read(@repo_paths_file) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, map} -> Map.get(map, repo, default_path)
          _ -> default_path
        end

      _ ->
        default_path
    end
  end
end
