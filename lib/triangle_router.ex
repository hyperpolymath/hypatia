# SPDX-License-Identifier: PMPL-1.0-or-later

defmodule Hypatia.TriangleRouter do
  @moduledoc """
  Routes findings through the Safety Triangle hierarchy.

  Safety Triangle: Eliminate > Substitute > Control

  For each pattern+repo combination:
  1. Can we eliminate? (recipe exists, confidence >= 0.90, auto_fixable)
  2. Can we substitute? (proven module exists, binding for repo language)
  3. Fall back to control (add guards, documentation, manual review)

  Confidence thresholds (from gitbot-fleet learning loop):
  - >= 0.95: auto-execute (robot-repo-automaton)
  - 0.85-0.94: execute with review (rhodibot creates PR)
  - < 0.85: report only (sustainabot advisory)
  """

  alias Hypatia.RecipeMatcher

  require Logger

  @auto_execute_threshold 0.95
  @review_threshold 0.85

  @doc """
  Route a single pattern to the appropriate triangle tier action.

  Returns one of:
  - {:eliminate, recipe, pattern} — hazard can be removed
  - {:substitute, recipe, pattern} — replace with proven-safe module
  - {:control, pattern} — add guards/documentation only
  """
  def route(pattern, repo, language) do
    category = Map.get(pattern, "category", "unknown")
    pattern_id = Map.get(pattern, "id", "unknown")
    tier = Map.get(pattern, "triangle_tier", "control")

    case tier do
      "eliminate" ->
        try_eliminate(pattern_id, pattern, language)

      "substitute" ->
        try_substitute(pattern_id, pattern, category, language)

      _ ->
        {:control, enrich_pattern(pattern, repo)}
    end
  end

  @doc """
  Route a batch of patterns for a repo+language combination.
  Returns a list of {tier, recipe_or_nil, pattern} tuples.
  """
  def route_batch(patterns, repo, language) do
    Enum.map(patterns, fn pattern ->
      route(pattern, repo, language)
    end)
  end

  @doc """
  Determine the dispatch strategy based on confidence level.

  Returns:
  - :auto_execute — confidence >= 0.95, robot-repo-automaton
  - :review — confidence 0.85-0.94, rhodibot creates PR
  - :report_only — confidence < 0.85, sustainabot advisory
  """
  def dispatch_strategy(confidence) when is_number(confidence) do
    cond do
      confidence >= @auto_execute_threshold -> :auto_execute
      confidence >= @review_threshold -> :review
      true -> :report_only
    end
  end

  def dispatch_strategy(_), do: :report_only

  # --- Private ---

  defp try_eliminate(pattern_id, pattern, language) do
    case RecipeMatcher.best_recipe(pattern_id, language) do
      nil ->
        # No recipe — check if any eliminate recipe exists for this pattern's category
        category = Map.get(pattern, "category", "unknown")

        case RecipeMatcher.substitution_for_category(category) do
          %{"triangle_tier" => "eliminate"} = sub ->
            # There's an eliminate substitution but no specific recipe yet
            synthetic_recipe = %{
              "id" => "synthetic-eliminate-#{category}",
              "triangle_tier" => "eliminate",
              "confidence" => Map.get(sub, "confidence", 0.5),
              "proven_module" => List.first(Map.get(sub, "proven_modules", [])),
              "auto_fixable" => false,
              "description" => Map.get(sub, "note", "Eliminate via #{category}")
            }

            {:eliminate, synthetic_recipe, pattern}

          _ ->
            # Fall through to substitute or control
            try_substitute(pattern_id, pattern, Map.get(pattern, "category", "unknown"), language)
        end

      recipe ->
        {:eliminate, recipe, pattern}
    end
  end

  defp try_substitute(pattern_id, pattern, category, language) do
    # First check if there's a direct recipe
    case RecipeMatcher.best_recipe(pattern_id, language) do
      %{"triangle_tier" => "substitute"} = recipe ->
        {:substitute, recipe, pattern}

      _ ->
        # Check proven-substitutions for this category
        case RecipeMatcher.substitution_for_category(category) do
          %{"proven_modules" => modules} = sub when is_list(modules) and modules != [] ->
            recipe = %{
              "id" => "proven-substitute-#{category}",
              "triangle_tier" => "substitute",
              "confidence" => Map.get(sub, "confidence", 0.5),
              "proven_module" => List.first(modules),
              "proven_modules" => modules,
              "auto_fixable" => false,
              "formally_proven" => Map.get(sub, "formally_proven", false),
              "description" => "Replace with proven/#{List.first(modules)}"
            }

            {:substitute, recipe, pattern}

          _ ->
            {:control, enrich_pattern(pattern, nil)}
        end
    end
  end

  defp enrich_pattern(pattern, repo) do
    pattern
    |> Map.put("routed_repo", repo)
    |> Map.put("severity_score", severity_to_score(Map.get(pattern, "severity", "Medium")))
  end

  defp severity_to_score("Critical"), do: 1.0
  defp severity_to_score("High"), do: 0.8
  defp severity_to_score("Medium"), do: 0.5
  defp severity_to_score("Low"), do: 0.2
  defp severity_to_score(_), do: 0.5
end
