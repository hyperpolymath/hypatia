# SPDX-License-Identifier: PMPL-1.0-or-later

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
  """

  alias Hypatia.TriangleRouter

  require Logger

  @verisimdb_path Path.expand("~/Documents/hyperpolymath-repos/verisimdb-data")

  @doc """
  Write a dispatch manifest from a list of routed actions.
  Returns {:ok, path, stats} on success.
  """
  def write(routed_actions) do
    dispatch_dir = Path.join(@verisimdb_path, "dispatch")
    File.mkdir_p!(dispatch_dir)

    manifest_path = Path.join(dispatch_dir, "pending.jsonl")

    # Also write a timestamped archive copy
    archive_name = "dispatch-#{Date.utc_today() |> Date.to_iso8601()}.jsonl"
    archive_path = Path.join(dispatch_dir, archive_name)

    lines =
      routed_actions
      |> Enum.flat_map(fn action ->
        case action_to_manifest_entry(action) do
          nil -> []
          entries when is_list(entries) -> entries
          entry -> [entry]
        end
      end)

    content = Enum.map_join(lines, "\n", fn entry ->
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
      "recipe_id" => Map.get(recipe, "id"),
      "confidence" => confidence,
      "auto_fixable" => Map.get(recipe, "auto_fixable", false),
      "fix_script" => Map.get(recipe, "fix_script"),
      "action" => Map.get(recipe, "action", "unknown"),
      "match" => Map.get(recipe, "match"),
      "replacement" => Map.get(recipe, "replacement"),
      "proven_module" => Map.get(recipe, "proven_module"),
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
      "recipe_id" => Map.get(recipe, "id"),
      "confidence" => confidence,
      "auto_fixable" => false,
      "proven_module" => Map.get(recipe, "proven_module"),
      "proven_modules" => Map.get(recipe, "proven_modules", []),
      "formally_proven" => Map.get(recipe, "formally_proven", false),
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
      "recipe_id" => nil,
      "confidence" => 0.0,
      "auto_fixable" => false,
      "timestamp" => DateTime.utc_now() |> DateTime.to_iso8601()
    }
  end

  defp action_to_manifest_entry(_), do: nil

  defp get_repo(pattern_or_finding) do
    Map.get(pattern_or_finding, "routed_repo") ||
      case Map.get(pattern_or_finding, "repos_affected_list", []) do
        [r | _] -> r
        _ -> "unknown"
      end
  end
end
